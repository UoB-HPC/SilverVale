#include <iostream>

#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/ASTUnit.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "agv/semantic_llvm.h"

using namespace clang;
using namespace aspartame;

std::vector<Decl *> agv::topLevelDeclsInMainFile(ASTUnit &unit) {
  std::vector<Decl *> xs;
  unit.visitLocalTopLevelDecls(&xs, [](auto xsPtr, auto decl) {
    reinterpret_cast<decltype(xs) *>(xsPtr)->push_back(const_cast<Decl *>(decl));
    return true;
  });
  return xs ^
         filter([&](Decl *d) { return unit.getSourceManager().isInMainFile(d->getLocation()); });
}

static inline CXXMethodDecl *extractLambdaCallMethodFromDeclRefTpe(Expr *expr) {
  if (auto ref = llvm::dyn_cast<DeclRefExpr>(expr->IgnoreUnlessSpelledInSource()); ref) {
    if (auto recordTpe = llvm::dyn_cast<RecordType>(ref->getType().getCanonicalType()); recordTpe) {

      if (auto cxxRecordDecl = llvm::dyn_cast<CXXRecordDecl>(recordTpe->getDecl());
          cxxRecordDecl && cxxRecordDecl->isLambda()) {

        return cxxRecordDecl->getLambdaCallOperator();
      }
    }
  }
  return {};
}

template <typename T, typename Node, typename... Fs> std::optional<T> visitDyn(Node n, Fs... fs) {
  std::optional<T> result{};
  [[maybe_unused]] auto _ = {[&]() {
    if (!result) {
      if (auto x = llvm::dyn_cast<std::remove_pointer_t<agv::arg0_t<Fs>>>(n)) { result = T(fs(x)); }
    }
    return 0;
  }()...};
  return result;
}
template <typename Node, typename... Fs> void visitDyn0(Node n, Fs... fs) {
  [[maybe_unused]] auto _ = ([&]() -> bool {
    if (auto x = llvm::dyn_cast<std::remove_pointer_t<agv::arg0_t<Fs>>>(n)) {
      fs(x);
      return true;
    }
    return false;
  }() || ...);
}

agv::ClangASTSemanticTreeVisitor::ClangASTSemanticTreeVisitor(
    agv::SemanticTree<std::string> *root, clang::ASTContext &Context,
    agv::ClangASTSemanticTreeVisitor::Option option)
    : SemanticTreeVisitor(root), Context(Context), option(std::move(option)) {}

bool agv::ClangASTSemanticTreeVisitor::TraverseStmt(clang::Stmt *stmt) { // NOLINT(*-no-recursion)
  // Remove implicits
  if (Expr *expr = llvm::dyn_cast_or_null<Expr>(stmt); expr) {
    stmt = expr->IgnoreUnlessSpelledInSource();
  }
  if (!stmt) return single("<<<NULL>>>");

  return visitDyn<bool>(
             stmt,
             [&](CompoundStmt *compound) {
               return compound->body() | forall([&](auto s) { return TraverseStmt(s); });
             },
             [&](DeclStmt *decl) {
               return decl->decls() | forall([&](auto s) { return TraverseDecl(s); });
             },
             [&](IntegerLiteral *lit) {
               return single(std::string(stmt->getStmtClassName()) + ": " +
                             std::to_string(lit->getValue().getLimitedValue()));
             },
             [&](FloatingLiteral *lit) {
               return single(std::string(stmt->getStmtClassName()) + ": " +
                             std::to_string(lit->getValue().convertToDouble()));
             },
             [&](StringLiteral *lit) {
               return single(std::string(stmt->getStmtClassName()) + ": \"" +
                             (lit->getString().str() ^ replace_all("\n", "\\n")) + "\"");
             }) ^
         fold([&]() {
           auto name = std::string(stmt->getStmtClassName());
           auto &sm = Context.getSourceManager();

           // Try to inline calls
           auto handleFn = [&](CallExpr *call, FunctionDecl *direct) {
             name += +": " + direct->getDeclName().getAsString();

             bool projectSymbol = option.roots | exists([&](auto root) {
                                    return sm.getFilename(direct->getLocation()).starts_with(root);
                                  });
             if (projectSymbol && direct->hasBody()) { // Symbol part of project root, inline

               //               node->children.emplace_back("Inline: " +
               //               direct->getDeclName().getAsString() +
               //                                           " @ " +
               //                                           sm.getFilename(direct->getLocation()).str());

               return TraverseStmt(direct->getBody());

             } else { // Symbol outside the project root, expand all executables here instead
                      //               node->children.emplace_back("No Inline: " +
                      //               direct->getDeclName().getAsString() +
                      //                                           " @ " +
               //                                           sm.getFilename(direct->getLocation()).str());
               return scoped(
                   [&]() {
                     return call->arguments() | forall([&](Expr *arg) {
                              // for DeclRefs to a lambda in a no-inline case, expand the body here
                              if (auto lambdaApply = extractLambdaCallMethodFromDeclRefTpe(arg);
                                  lambdaApply) {
                                node->children.emplace_back("App: " +
                                                            lambdaApply->getNameAsString());
                                return TraverseStmt(lambdaApply->getBody());
                              } else {
                                return TraverseStmt(arg);
                              }
                            });
                   },
                   name);
             }
           };

           if (auto *call = llvm::dyn_cast_or_null<CallExpr>(stmt); option.inlineCalls && call) {
             // Call to a real function decl in the project, try to inline
             if (auto *overload = llvm::dyn_cast_or_null<OverloadExpr>(call->getCallee());
                 overload) {
               auto direct = overload->decls() |
                             collect([](NamedDecl *x) -> std::optional<FunctionDecl *> {
                               if (auto fn = llvm::dyn_cast_or_null<FunctionDecl>(x); fn) return fn;
                               return {};
                             }) |
                             head_maybe();
               if (direct) { return handleFn(call, *direct); }
             } else if (auto direct = call->getDirectCallee(); direct) {
               return handleFn(call, direct);
             } else {

               // Call to non-functions:
               //  lambda apply, unapplied template fns, unresolved ADL calls
             }
           }

           auto suffix = visitDyn<std::string>(
               stmt, //
               [&](BinaryOperator *bin) { return bin->getOpcodeStr().str(); },
               [&](DeclRefExpr *ref) {
                 return option.normaliseVarName ? "" : ref->getDecl()->getDeclName().getAsString();
               });

           return scoped(
               [&]() {
                 return RecursiveASTVisitor<ClangASTSemanticTreeVisitor>::TraverseStmt(stmt);
               },
               suffix ^ map([&](auto s) { return name + ": " + s; }) ^ get_or_else(name));
         });
}

bool agv::ClangASTSemanticTreeVisitor::TraverseDecl(clang::Decl *decl) { // NOLINT(*-no-recursion)
  if (!decl) return true;
  auto suffix = visitDyn<std::string>(
      decl, //
      [&](FunctionDecl *fn) {
        return option.normaliseFnName ? "" : fn->getDeclName().getAsString();
      },
      [&](VarDecl *var) {
        return option.normaliseVarName ? "" : var->getDeclName().getAsString();
      });
  std::string name = decl->getDeclKindName();
  return scoped(
      [&]() { return RecursiveASTVisitor<ClangASTSemanticTreeVisitor>::TraverseDecl(decl); },
      suffix ^ map([&](auto s) { return name + ": " + s; }) ^ get_or_else(name));
}

static std::string print(llvm::Type *t) {
  std::string s;
  llvm::raw_string_ostream rso(s);
  t->print(rso);
  return rso.str();
}

agv::LLVMIRTreeVisitor::LLVMIRTreeVisitor(agv::SemanticTree<std::string> *root,
                                          const llvm::Module &module, bool normaliseName)
    : SemanticTreeVisitor(root), normaliseName(normaliseName) {

  module.globals() | for_each([&](const llvm::GlobalVariable &var) {
    auto name = named("Global", var.getName().str() + ": " + print(var.getType()));
    if (var.hasInitializer()) {
      scoped([&]() { walk(var.getInitializer()); }, name);
    } else {
      single(name);
    }
  });
  module.functions() | for_each([&](const llvm::Function &fn) {
    auto args =
        fn.args() | mk_string(",", [&](const llvm::Argument &arg) { return print(arg.getType()); });
    auto sig =
        ((!normaliseName ? fn.getName() : "") + "(" + args + "): " + print(fn.getReturnType()))
            .str();

    scoped(
        [&]() {
          fn | for_each([&](const llvm::BasicBlock &bb) {
            scoped(
                [&]() {
                  bb | for_each([&](const llvm::Instruction &ins) { walk(&ins); });
                  return true;
                },
                named("BB", bb.getName().str()));
          });
        },
        "Fn: " + sig);
  });
}

std::string agv::LLVMIRTreeVisitor::named(const std::string &kind, const std::string &name) const {
  return normaliseName ? kind : (kind + ": " + name);
}

void agv::LLVMIRTreeVisitor::walk(const llvm::Value *value) {
  if (!value) {
    single("<<<NUll>>>");
    return;
  }
  visitDyn0( //
      value, //
      [&](const llvm::Argument *arg) { return single("Arg: " + print(arg->getType())); },
      [&](const llvm::Instruction *ins) {
        std::string s;
        llvm::raw_string_ostream rso(s);
        ins->print(rso);

        auto args = ins->operands() |
                    mk_string(",", [](const llvm::Use &u) { return print(u->getType()); });
        auto sig = std::string(ins->getOpcodeName()) + "(" + args + "):" + print(ins->getType());
        //        scoped( //
        //            [&]() {
        //              ins->operand_values() | zip_with_index() |
        //                  for_each([&](const llvm::Value *v, auto idx) {
        //                    if (idx == 0) return; //  arg 0 should be the ins itself
        //                    if (v == value) single("(identity)");
        //                    else walk(v);
        //                  });
        //            },
        //            sig);
        single(sig);
      },
      [&](const llvm::Function *c) {
        single(named("Fn", c->getName().str() + ": " + print(c->getType())));
      },
      [&](const llvm::Constant *c) {
        std::string s;
        llvm::raw_string_ostream rso(s);
        c->print(rso);
        single(named("Const", rso.str() + ": " + print(c->getType())));
      },
      [&](const llvm::Value *c) {
        single(named("Val", c->getName().str() + ": " + print(c->getType())));
      }

  );
}

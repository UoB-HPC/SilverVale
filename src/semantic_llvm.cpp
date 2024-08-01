#include <iostream>

#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/ASTUnit.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "fmt/core.h"

#include "sv/semantic_llvm.h"

using namespace clang;
using namespace aspartame;

std::vector<Decl *> sv::topLevelDeclsInMainFile(ASTUnit &unit) {
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
      if (auto x = llvm::dyn_cast<std::remove_pointer_t<sv::arg0_t<Fs>>>(n)) { result = T(fs(x)); }
    }
    return 0;
  }()...};
  return result;
}
template <typename Node, typename... Fs> void visitDyn0(Node n, Fs... fs) {
  [[maybe_unused]] auto _ = ([&]() -> bool {
    if (auto x = llvm::dyn_cast<std::remove_pointer_t<sv::arg0_t<Fs>>>(n)) {
      fs(x);
      return true;
    }
    return false;
  }() || ...);
}

sv::ClangASTSemanticTreeVisitor::ClangASTSemanticTreeVisitor(
    NTree<SNode> *root, clang::ASTContext &Context,
    ClangASTSemanticTreeVisitor::Option option)
    : NTreeVisitor(root), Context(Context), option(std::move(option)) {}

[[nodiscard]] sv::Location
sv::ClangASTSemanticTreeVisitor::locationOf(const clang::SourceLocation &l) const {
  SourceManager &mgr = Context.getSourceManager();
  return {.filename = std::filesystem::path(mgr.getFilename(l).str()).filename(),
          .line = mgr.getSpellingLineNumber(l),
          .col = mgr.getSpellingColumnNumber(l)};
}

bool sv::ClangASTSemanticTreeVisitor::TraverseStmt(clang::Stmt *stmt) { // NOLINT(*-no-recursion)
  // Remove implicits
  if (Expr *expr = llvm::dyn_cast_or_null<Expr>(stmt); expr) {
    stmt = expr->IgnoreUnlessSpelledInSource();
  }
  if (!stmt) return single(SNode{"<<<NULL>>>", Location{}});

  return visitDyn<bool>(
             stmt,
             [&](CompoundStmt *compound) {                    // NOLINT(*-no-recursion)
               return compound->body() | forall([&](auto &s) { // NOLINT(*-no-recursion)
                        return TraverseStmt(s);
                      });
             },
             [&](DeclStmt *decl) {                         // NOLINT(*-no-recursion)
               return decl->decls() | forall([&](auto &s) { // NOLINT(*-no-recursion)
                        return TraverseDecl(s);
                      });
             },
             [&](IntegerLiteral *lit) { // NOLINT(*-no-recursion)
               return single(SNode{fmt::format("{}: {}", stmt->getStmtClassName(),
                                               lit->getValue().getLimitedValue()),
                                   locationOf(lit->getLocation())});
             },
             [&](FloatingLiteral *lit) { // NOLINT(*-no-recursion)
               return single(SNode{fmt::format("{}: {}", stmt->getStmtClassName(),
                                               lit->getValue().convertToDouble()),
                                   locationOf(lit->getLocation())});
             },
             [&](StringLiteral *lit) { // NOLINT(*-no-recursion)
               return single(SNode{fmt::format("{}: \"{}\"", stmt->getStmtClassName(),
                                               (lit->getString().str() ^ replace_all("\n", "\\n"))),
                                   locationOf(lit->getBeginLoc())});
             }) ^
         fold([&]() { // NOLINT(*-no-recursion)
           auto name = std::string(stmt->getStmtClassName());
           auto &sm = Context.getSourceManager();

           // Try to inline calls
           auto handleFn = [&](CallExpr *call, FunctionDecl *direct) { // NOLINT(*-no-recursion)
             name += +": " + direct->getDeclName().getAsString();

             bool projectSymbol = option.roots | exists([&](auto &root) {
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
                   [&]() {                                              // NOLINT(*-no-recursion)
                     return call->arguments() | forall([&](Expr *arg) { // NOLINT(*-no-recursion)
                              // for DeclRefs to a lambda in a no-inline case, expand the body here
                              if (auto lambdaApply = extractLambdaCallMethodFromDeclRefTpe(arg);
                                  lambdaApply) {
                                node->children.emplace_back(
                                    SNode{fmt::format("App: {}", lambdaApply->getNameAsString()),
                                          locationOf(arg->getExprLoc())});
                                return TraverseStmt(lambdaApply->getBody());
                              } else {
                                return TraverseStmt(arg);
                              }
                            });
                   },
                   SNode{name, locationOf(direct->getLocation())});
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
               [&]() { // NOLINT(*-no-recursion)
                 return RecursiveASTVisitor<ClangASTSemanticTreeVisitor>::TraverseStmt(stmt);
               },
               SNode{suffix ^ fold([&](auto s) { return fmt::format("{}: {}", name, s); },
                                   [&]() { return name; }),
                     locationOf(stmt->getBeginLoc())});
         });
}

bool sv::ClangASTSemanticTreeVisitor::TraverseDecl(clang::Decl *decl) { // NOLINT(*-no-recursion)
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
      [&]() { // NOLINT(*-no-recursion)
        return RecursiveASTVisitor<ClangASTSemanticTreeVisitor>::TraverseDecl(decl);
      },
      SNode{suffix ^ fold([&](auto s) { return fmt::format("{}: {}", name, s); },
                          [&]() { return name; }),
            locationOf(decl->getLocation())});
}

static std::string print(llvm::Type *t) {
  std::string s;
  llvm::raw_string_ostream rso(s);
  t->print(rso);
  return rso.str();
}

[[nodiscard]] static sv::Location resolveFnLocation(const llvm::Function &fn) {
  if (auto prog = fn.getSubprogram()) {
    return sv::Location{.filename = std::filesystem::path(prog->getFilename().str()).filename(),
                        .line = prog->getLine(),
                        .col = 0};
  }
  return {};
}

sv::LLVMIRTreeVisitor::LLVMIRTreeVisitor(NTree<SNode> *root, const llvm::Module &module,
                                         bool normaliseName)
    : NTreeVisitor(root), normaliseName(normaliseName) {

  module.globals() | for_each([&](const llvm::GlobalVariable &var) {
    Location loc{};

    if (auto md = var.getMetadata(llvm::LLVMContext::MD_dbg)) {
      if (auto expr = llvm::dyn_cast<llvm::DIGlobalVariableExpression>(md)) {
        if (auto diVar = expr->getVariable()) {
          loc.filename = std::filesystem::path(diVar->getFilename().str()).filename();
          loc.line = diVar->getLine();
        }
      }
    }

    auto name = named("Global", var.getName().str() + ": " + print(var.getType()));
    if (var.hasInitializer()) {
      scoped([&]() { walk(var.getInitializer()); }, SNode{name, loc});
    } else {
      single(SNode{name, loc});
    }
  });
  module.functions() | for_each([&](const llvm::Function &fn) {
    auto args =
        fn.args() | mk_string(",", [&](const llvm::Argument &arg) { return print(arg.getType()); });
    auto sig = fmt::format("{}({}): {}", (!normaliseName ? fn.getName().str() : ""), args,
                           print(fn.getReturnType()));

    scoped(
        [&]() {
          fn | for_each([&](const llvm::BasicBlock &bb) {
            scoped(
                [&]() {
                  bb | for_each([&](const llvm::Instruction &ins) { walk(&ins); });
                  return true;
                },
                SNode{named("BB", bb.getName().str()), Location{}});
          });
        },
        SNode{fmt::format("Fn: {}", sig), resolveFnLocation(fn)});
  });
}

std::string sv::LLVMIRTreeVisitor::named(const std::string &kind, const std::string &name) const {
  return normaliseName ? kind : (kind + ": " + name);
}

void sv::LLVMIRTreeVisitor::walk(const llvm::Value *value) {
  if (!value) {
    single(SNode{"<<<NUll>>>", Location{}});
    return;
  }
  visitDyn0( //
      value, //
      [&](const llvm::Argument *arg) {
        return single(SNode{fmt::format("Arg: {}", print(arg->getType())), Location{}});
      },
      [&](const llvm::Function *fn) {
        single(SNode{named("Fn", fmt::format("{}: {}", fn->getName().str(), print(fn->getType()))),
                     resolveFnLocation(*fn)});
      },
      [&](const llvm::Instruction *ins) {
        if (ins->isDebugOrPseudoInst()) { return; }
        //        if (auto call = llvm::dyn_cast<llvm::CallInst>(ins)) {
        //          if (auto func = call->getCalledFunction(); func && func->isIntrinsic()) {
        //            switch (func->getIntrinsicID()) {
        //              case llvm::Intrinsic::dbg_assign: [[fallthrough]];
        //              case llvm::Intrinsic::dbg_declare: [[fallthrough]];
        //              case llvm::Intrinsic::dbg_label: [[fallthrough]];
        //              case llvm::Intrinsic::dbg_value: return; // found a debug intrinsic, stop
        //              default: break;                          // keep going otherwise
        //            }
        //          }
        //        }

        std::string s;
        llvm::raw_string_ostream rso(s);
        ins->print(rso);

        Location loc{};
        if (auto &dloc = ins->getDebugLoc(); dloc) {
          auto actualDI = (iterate(dloc.get(), [](auto l) { return l->getInlinedAt(); }) //
                           | take_while([](auto l) { return l != nullptr; })             //
                           | last_maybe()) ^
                          get_or_else(dloc.get());
          loc.filename = std::filesystem::path(actualDI->getFilename().str()).filename();
          loc.line = actualDI->getLine();
          loc.col = actualDI->getColumn();
        }

        auto args = ins->operands() |
                    mk_string(",", [](const llvm::Use &u) { return print(u->getType()); });

        auto sig = fmt::format("{}({}):{}", ins->getOpcodeName(), args, print(ins->getType()));

        //        scoped( //
        //            [&]() {
        //              ins->operand_values() | zip_with_index() |
        //                  for_each([&](const llvm::Value *v, auto idx) {
        //                    if (idx == 0) return; // arg 0 should be the ins itself
        //                    if (v == value) single("(identity)");
        //                    else walk(v);
        //                  });
        //            },
        //            sig);

        single(SNode{sig, loc});
      },
      [&](const llvm::Constant *c) {
        std::string s;
        llvm::raw_string_ostream rso(s);
        c->print(rso);
        single(SNode{named("Const", fmt::format("{}: {}", rso.str(), print(c->getType()))),
                     Location{}});
      },
      [&](const llvm::Value *c) {
        single(SNode{named("Val", fmt::format("{}: {}", c->getName().str(), print(c->getType()))),
                     Location{}});
      }
  );
}

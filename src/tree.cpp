#include <iostream>

#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/ASTUnit.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "p3md/tree.h"

using namespace clang;
using namespace aspartame;

std::vector<Decl *> p3md::topLevelDeclsInMainFile(ASTUnit &unit) {
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

p3md::ClangASTSemanticTreeVisitor::ClangASTSemanticTreeVisitor(
    p3md::SemanticTree<std::string> *root, clang::ASTContext &Context,
    p3md::ClangASTSemanticTreeVisitor::Option option)
    : SemanticTreeVisitor(root), Context(Context), option(std::move(option)) {}

bool p3md::ClangASTSemanticTreeVisitor::TraverseStmt(clang::Stmt *stmt) { // NOLINT(*-no-recursion)
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

bool p3md::ClangASTSemanticTreeVisitor::TraverseDecl(clang::Decl *decl) { // NOLINT(*-no-recursion)
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

p3md::TsTree::TsTree() = default;
p3md::TsTree::TsTree(const std::string &source, const TSLanguage *lang)
    : source(source),
      parser(std::shared_ptr<TSParser>(ts_parser_new(), [](auto x) { ts_parser_delete(x); })) {
  ts_parser_set_language(parser.get(), lang);
  tree = std::shared_ptr<TSTree>(
      ts_parser_parse_string(parser.get(), nullptr, source.c_str(), source.size()),
      [](auto x) { ts_tree_delete(x); });
}

TSNode p3md::TsTree::root() const { return ts_tree_root_node(tree.get()); }

p3md::TsTree p3md::TsTree::deleteNodes(const std::string &type,
                                       const std::optional<TSNode> &node) const {
  size_t offset = 0;
  std::string out = source;
  deleteNodes(node ? *node : root(), type, offset, out);
  return {out, ts_parser_language(parser.get())};
}
void p3md::TsTree::deleteNodes( // NOLINT(*-no-recursion)
    const TSNode &node, const std::string &type, size_t &offset, std::string &out) {
  if (std::string(ts_node_type(node)) == type) {
    auto start = ts_node_start_byte(node);
    auto end = ts_node_end_byte(node);
    out.erase(start - offset, end - start);
    offset += end - start;
  } else {
    for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
      deleteNodes(ts_node_child(node, i), type, offset, out);
    }
  }
}

p3md::TsTree p3md::TsTree::normaliseWhitespaces(size_t maxWhitespaces,
                                                const std::optional<TSNode> &node) const {
  size_t offset = 0;
  std::string out = source;
  normaliseWhitespaces(node ? *node : root(), offset, maxWhitespaces, out);
  return {out ^ trim(), ts_parser_language(parser.get())};
}
void p3md::TsTree::normaliseWhitespaces( // NOLINT(*-no-recursion)
    const TSNode &node, size_t &offset, size_t maxWhitespaces, std::string &out) {
  auto count = ts_node_child_count(node);
  for (uint32_t i = 0; i < count; ++i) {
    auto current = ts_node_child(node, i);
    if (i + 1 < count) {
      auto next = ts_node_child(node, i + 1);
      auto start = ts_node_end_byte(current);
      auto end = ts_node_start_byte(next);
      if (end - start > 0) {

        // TODO
        std::cout << "Drop `" << out.substr(start - offset, end - start) << "`\n";

        //        out.erase(start - offset, end - start);
        //        out.replace(start - offset, end - start, std::string(end - start, '@'));
        //        offset += end - start;
      }
    }
    normaliseWhitespaces(current, offset, maxWhitespaces, out);
  }
}

size_t p3md::TsTree::sloc(const std::optional<TSNode> &node) const {
  std::unordered_set<uint32_t> rowsWithNode;
  walk(
      [&](auto x) {
        rowsWithNode.emplace(ts_node_start_point(x).row);
        rowsWithNode.emplace(ts_node_end_point(x).row);
        return true;
      },
      node);
  return rowsWithNode.size();
}

size_t p3md::TsTree::lloc(const std::optional<TSNode> &node) const {
  size_t lloc = 0;
  walk(
      [&](auto x) {
        std::string type = ts_node_type(x);
        // don't count compound stmt as it contains children
        if (type != "compound_statement" && (type ^ ends_with("declaration") || //
                                             type ^ ends_with("statement") ||   //
                                             type ^ ends_with("directive"))     //
        )
          lloc++;
        return true;
      },
      node);
  return lloc;
}

static std::string print(llvm::Type *t) {
  std::string s;
  llvm::raw_string_ostream rso(s);
  t->print(rso);
  return rso.str();
}

p3md::LLVMIRTreeVisitor::LLVMIRTreeVisitor(p3md::SemanticTree<std::string> *root,
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

std::string p3md::LLVMIRTreeVisitor::named(const std::string &kind, const std::string &name) const {
  return normaliseName ? kind : (kind + ": " + name);
}

void p3md::LLVMIRTreeVisitor::walk(const llvm::Value *value) {
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

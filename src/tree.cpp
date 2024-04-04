#include "clang/Basic/SourceManager.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "p3md/tree.h"

using namespace clang;
using namespace aspartame;

p3md::TreeSemanticVisitor::TreeSemanticVisitor(p3md::SemanticNode<std::string> *root,
                                               clang::ASTContext &Context,
                                               p3md::TreeSemanticVisitor::Option option)
    : node(root), Context(Context), option(std::move(option)) {}
bool p3md::TreeSemanticVisitor::TraverseStmt(clang::Stmt *stmt) { // NOLINT(*-no-recursion)
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

               node->children.emplace_back("Inline: " + direct->getDeclName().getAsString() +
                                           " @ " + sm.getFilename(direct->getLocation()).str());

               return TraverseStmt(direct->getBody());

             } else { // Symbol outside the project root, expand all executables here instead
               node->children.emplace_back("No Inline: " + direct->getDeclName().getAsString() +
                                           " @ " + sm.getFilename(direct->getLocation()).str());

               return scoped(
                   [&]() {

                     return RecursiveASTVisitor<TreeSemanticVisitor>::TraverseStmt(stmt);
//                     return call->arguments() | forall([&](Expr* arg) {
//                                                            node->children.emplace_back("Arg kind:"
//                                                            + std::string (arg->getStmtClassName()));
//
//                              return TraverseStmt(arg);
//                            });
                   },
                   name);
             }
           };

           if (auto *call = llvm::dyn_cast_or_null<CallExpr>(stmt); call) {
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
               [&]() { return RecursiveASTVisitor<TreeSemanticVisitor>::TraverseStmt(stmt); },
               suffix ^ map([&](auto s) { return name + ": " + s; }) ^ get_or_else(name));
         });
}
bool p3md::TreeSemanticVisitor::TraverseDecl(clang::Decl *decl) { // NOLINT(*-no-recursion)
  auto suffix = visitDyn<std::string>(
      decl, //
      [&](FunctionDecl *fn) {
        return option.normaliseFnName ? "" : fn->getDeclName().getAsString();
      },
      [&](VarDecl *var) {
        return option.normaliseVarName ? "" : var->getDeclName().getAsString();
      });
  std::string name = decl->getDeclKindName();
  return scoped([&]() { return RecursiveASTVisitor<TreeSemanticVisitor>::TraverseDecl(decl); },
                suffix ^ map([&](auto s) { return name + ": " + s; }) ^ get_or_else(name));
}

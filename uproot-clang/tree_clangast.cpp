#include <iostream>

#include "clang/Basic/SourceManager.h"
#include "clang/Frontend/ASTUnit.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "fmt/core.h"

#include "tree_clangast.h"
#include "tree_utils.h"

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

sv::ClangASTSemanticTreeVisitor::ClangASTSemanticTreeVisitor(
    NTree<SNode> *root, clang::ASTContext &Context, ClangASTSemanticTreeVisitor::Option option)
    : NTreeVisitor(root), Context(Context), option(std::move(option)) {}

[[nodiscard]] sv::Location
sv::ClangASTSemanticTreeVisitor::locationOf(const clang::SourceLocation &l) const {
  SourceManager &mgr = Context.getSourceManager();
  return {.path = std::filesystem::path(mgr.getFilename(l).str()),
          .line = mgr.getSpellingLineNumber(l),
          .col = mgr.getSpellingColumnNumber(l)};
}

bool sv::ClangASTSemanticTreeVisitor::TraverseStmt(clang::Stmt *stmt) { // NOLINT(*-no-recursion)
  if (Expr *expr = llvm::dyn_cast_or_null<Expr>(stmt); expr) {          // Remove implicits
    stmt = expr->IgnoreUnlessSpelledInSource();
  }
  if (!stmt) {
    single(SNode{"<<<NULL>>>", Location{}});
    return true;
  }

  visitDyn0(
      stmt,
      [&](CompoundStmt *compound) {                // NOLINT(*-no-recursion)
        compound->body() | for_each([&](auto &s) { // NOLINT(*-no-recursion)
          TraverseStmt(s);
        });
      },
      [&](DeclStmt *decl) {                     // NOLINT(*-no-recursion)
        decl->decls() | for_each([&](auto &s) { // NOLINT(*-no-recursion)
          TraverseDecl(s);
        });
      },
      [&](IntegerLiteral *lit) { // NOLINT(*-no-recursion)
        single(SNode{
            fmt::format("{}: {}", stmt->getStmtClassName(), lit->getValue().getLimitedValue()),
            locationOf(lit->getLocation())});

      },
      [&](FloatingLiteral *lit) { // NOLINT(*-no-recursion)
        llvm::SmallVector<char> floatRepr;
        lit->getValue().toString(floatRepr);
        single(SNode{fmt::format("{}: {}", stmt->getStmtClassName(),
                                 std::string(floatRepr.begin(), floatRepr.end())),
                     locationOf(lit->getLocation())});

      },
      [&](StringLiteral *lit) { // NOLINT(*-no-recursion)
        single(SNode{fmt::format("{}: \"{}\"", stmt->getStmtClassName(),
                                 (lit->getString().str() ^ replace_all("\n", "\\n"))),
                     locationOf(lit->getBeginLoc())});
      },
      [&](Stmt *stmt) {
        auto name = std::string(stmt->getStmtClassName());
        auto &sm = Context.getSourceManager();

        // Try to inline calls
        auto handleFn = [&](CallExpr *call, FunctionDecl *direct) { // NOLINT(*-no-recursion)
          name += +": " + direct->getDeclName().getAsString();
          bool projectSymbol =
              option.roots | exists([&](auto &root) {
                return sm.getFilename(direct->getLocation()).str() ^ starts_with(root);
              });
          if (projectSymbol && direct->hasBody()) { // Symbol part of project root, inline
            TraverseStmt(direct->getBody());
          } else { // Symbol outside the project root, expand all executables here instead
            scoped(
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
          if (auto *overload = llvm::dyn_cast_or_null<OverloadExpr>(call->getCallee()); overload) {
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
            // Call to non-functions, ignore:
            //  lambda apply, unapplied template fns, unresolved ADL calls
          }
        }
        auto suffix = visitDyn<std::string>(
            stmt, //
            [&](BinaryOperator *bin) { return bin->getOpcodeStr().str(); },
            [&](DeclRefExpr *ref) {
              return option.normaliseVarName ? "" : ref->getDecl()->getDeclName().getAsString();
            });
        scoped(
            [&]() {                                      // NOLINT(*-no-recursion)
              stmt->children() | for_each([&](auto &s) { // NOLINT(*-no-recursion)
                TraverseStmt(s);
              });
            },
            SNode{suffix ^ fold([&](auto s) { return fmt::format("{}: {}", name, s); },
                                [&]() { return name; }),
                  locationOf(stmt->getBeginLoc())});
      });
  return true;
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
  scoped(
      [&]() { // NOLINT(*-no-recursion)
        RecursiveASTVisitor<ClangASTSemanticTreeVisitor>::TraverseDecl(decl);
      },
      SNode{suffix ^ fold([&](auto s) { return fmt::format("{}: {}", name, s); },
                          [&]() { return name; }),
            locationOf(decl->getLocation())});
  return true;
}

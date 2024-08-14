#pragma once

#include "sv/tree.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/ASTUnit.h"

namespace sv {

std::vector<clang::Decl *> topLevelDeclsInMainFile(clang::ASTUnit &unit);

class ClangASTSemanticTreeVisitor : private NTreeVisitor<SNode, void>,
                                    public clang::RecursiveASTVisitor<ClangASTSemanticTreeVisitor> {

public:
  struct Option {
    bool inlineCalls;
    bool normaliseVarName;
    bool normaliseFnName;
    std::vector<std::string> roots;
  };

private:
  clang::ASTContext &Context;
  Option option;
  [[nodiscard]] sv::Location locationOf(const clang::SourceLocation &e) const;

public:
  ClangASTSemanticTreeVisitor(NTree<SNode> *root, clang::ASTContext &Context, Option option);
  bool TraverseDecl(clang::Decl *decl);
  bool TraverseStmt(clang::Stmt *stmt);
};

} // namespace sv

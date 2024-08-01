#pragma once

#include "tree.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/ASTUnit.h"
#include "llvm/IR/Module.h"

namespace sv {

std::vector<clang::Decl *> topLevelDeclsInMainFile(clang::ASTUnit &unit);

class LLVMIRTreeVisitor : private NTreeVisitor<SNode, void> {
  bool normaliseName;
  [[nodiscard]] std::string named(const std::string &kind, const std::string &name) const;
  void walk(const llvm::Value *fn);

public:
  LLVMIRTreeVisitor(NTree<SNode> *root, const llvm::Module &module,
                    bool normaliseName);
};

class ClangASTSemanticTreeVisitor : private NTreeVisitor<SNode, bool>,
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
  [[nodiscard ]]  sv::Location  locationOf(const clang::SourceLocation &e) const ;


public:
  ClangASTSemanticTreeVisitor(NTree<SNode> *root, clang::ASTContext &Context,
                              Option option);
  bool TraverseDecl(clang::Decl *decl);
  bool TraverseStmt(clang::Stmt *stmt);
};

} // namespace sv

#pragma once

#include "tree.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/ASTUnit.h"
#include "llvm/IR/Module.h"

namespace sv {

std::vector<clang::Decl *> topLevelDeclsInMainFile(clang::ASTUnit &unit);

class LLVMIRTreeVisitor : private SemanticTreeVisitor<std::string, void> {
  bool normaliseName;
  [[nodiscard]] std::string named(const std::string &kind, const std::string &name) const;
  void walk(const llvm::Value *fn);

public:
  LLVMIRTreeVisitor(SemanticTree<std::string> *root, const llvm::Module &module,
                    bool normaliseName);
};

class ClangASTSemanticTreeVisitor : private SemanticTreeVisitor<std::string, bool>,
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

public:
  ClangASTSemanticTreeVisitor(SemanticTree<std::string> *root, clang::ASTContext &Context,
                              Option option);
  bool TraverseDecl(clang::Decl *decl);
  bool TraverseStmt(clang::Stmt *stmt);
};

} // namespace sv

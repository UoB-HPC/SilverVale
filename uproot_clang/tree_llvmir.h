#pragma once

#include "sv/tree.h"

#include "llvm/IR/Module.h"

namespace sv {

class LLVMIRTreeVisitor : private NTreeVisitor<SNode, void> {
  bool normaliseName;
  [[nodiscard]] std::string named(const std::string &kind, const std::string &name) const;
  void walk(const llvm::Value *fn);

public:
  LLVMIRTreeVisitor(NTree<SNode> *root, const llvm::Module &module, bool normaliseName);
};

} // namespace sv

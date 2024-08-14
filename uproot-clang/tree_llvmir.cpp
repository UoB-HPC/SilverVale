#include <iostream>

#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"

#include "aspartame/optional.hpp"
#include "aspartame/view.hpp"

#include "fmt/core.h"

#include "tree_llvmir.h"
#include "tree_utils.h"

using namespace aspartame;

static std::string print(llvm::Type *t) {
  std::string s;
  llvm::raw_string_ostream rso(s);
  t->print(rso);
  return rso.str();
}

[[nodiscard]] static sv::Location resolveFnLocation(const llvm::Function &fn) {
  if (auto prog = fn.getSubprogram()) {
    return sv::Location{.path = std::filesystem::path(prog->getFilename().str()),
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
          loc.path = std::filesystem::path(diVar->getFilename().str());
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
          loc.path = std::filesystem::path(actualDI->getFilename().str());
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
      });
}

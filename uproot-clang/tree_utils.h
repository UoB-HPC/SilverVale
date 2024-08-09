#pragma once

#include "sv/tree.h"

#include "llvm/Support/Casting.h"

namespace sv {

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

} // namespace sv

#pragma once

#include "oneapi/tbb.h"

namespace sv {

inline tbb::global_control par_setup(size_t n) {
  return {tbb::global_control::max_allowed_parallelism, n};
}

template <typename C, typename F> static void par_for(C &&xs, F f) {
  tbb::parallel_for(size_t{}, xs.size(), [&](auto idx) { f(xs[idx], idx); });
}

template <typename C, typename F> static auto par_map(C &&xs, F f) {
  using R = decltype(f(xs[size_t{}]));
  std::vector<R> out(xs.size());
  tbb::parallel_for(size_t{}, xs.size(), [&](auto idx) { out[idx] = std::move(f(xs[idx])); });
  return out;
}

} // namespace sv

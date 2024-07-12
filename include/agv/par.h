#pragma once

#include "oneapi/tbb.h"

namespace agv {

inline tbb::global_control par_setup(size_t n) {
  return {tbb::global_control::max_allowed_parallelism, n};
}

template <typename F> static void par_for(size_t n, F f) {
  tbb::parallel_for(size_t{}, n, [&](auto idx) { f(idx); });
}

template <typename C, typename F> static void par_for(C &&xs, F f) {
  tbb::parallel_for(size_t{}, xs.size(), [&](auto idx) { f(xs[idx], idx); });
}

} // namespace agv

#pragma once

#include "BS_thread_pool.hpp"

namespace sv {

namespace {
 BS::thread_pool pool;
}
inline void par_setup(size_t n) { pool.reset(n); }

template <typename C, typename F> static void par_for(C &&xs, F f) {
  pool.detach_blocks (size_t{}, xs.size(), [&](size_t start, size_t end) {
    for (size_t i = start; i < end; ++i)
      f(xs[i], i);
  });
  pool.wait();
}

template <typename C, typename F> static auto par_map(C &&xs, F f) {
  return pool.template submit_sequence (size_t{}, xs.size(), [&](size_t idx) { return f(xs[idx]); }).get();
}

template <typename C, typename F> static auto par_map(size_t N, C &&xs, F f) {
  BS::thread_pool localPool(N);
  return localPool.template submit_sequence (size_t{}, xs.size(), [&](size_t idx) { return f(xs[idx]); }).get();
}

} // namespace sv

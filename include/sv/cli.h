#pragma once

#include "spdlog/spdlog.h"
#include <atomic>
#include <iostream>
#include <string>

#include "fmt/format.h"
#include "fmt/std.h"

#ifdef __cpp_lib_syncbuf
  #include <syncstream>
  #define SV_COUT std::osyncstream(std::cout)
  #define SV_CERR std::osyncstream(std::cerr)
#else
  #include <iostream>
  #define SV_COUT std::cout
  #define SV_CERR std::cerr
#endif

// #define SV_WARNF(...) (SV_CERR << fmt::format("# Warning: " __VA_ARGS__) << std::endl)
// #define SV_INFOF(...) (SV_COUT << fmt::format("# " __VA_ARGS__) << std::endl)
// #define SV_ERRF(...) (SV_CERR << fmt::format("FATAL: " __VA_ARGS__) << std::endl)

#define SV_WARNF(...) spdlog::warn(__VA_ARGS__)
#define SV_INFOF(...) spdlog::info(__VA_ARGS__)
#define SV_ERRF(...) spdlog::error(__VA_ARGS__)

namespace sv {

class ProgressLogger {
  size_t total;
  int maxLogLength;
  std::atomic_size_t completed{1};

public:
  ProgressLogger(size_t total, int maxLogLength) : total(total), maxLogLength(maxLogLength) {}

  void log(const std::string &line, bool progress = true) {
    auto text =
        fmt::format("# [{}/{}] {:<{}}", //
                    (progress ? completed++ : completed.load()), total, line, maxLogLength + 10);
    if (progress) (SV_COUT << text << "\r").flush();
    else SV_COUT << text << std::endl;
  }
};

} // namespace sv
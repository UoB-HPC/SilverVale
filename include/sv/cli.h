#pragma once

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

#define SV_WARNF(...) (SV_CERR << fmt::format("# Warning: " __VA_ARGS__) << std::endl)
#define SV_INFOF(...) (SV_COUT << fmt::format("# " __VA_ARGS__) << std::endl)
#define SV_ERRF(...) (SV_CERR << fmt::format(__VA_ARGS__) << std::endl)

namespace sv {

class ProgressLogger {
  size_t total;
  int maxLogLength;
  std::atomic_size_t completed{1};

public:
  ProgressLogger(size_t total, int maxLogLength) : total(total), maxLogLength(maxLogLength) {}

  void log(const std::string &line, bool progress = true) {
    auto s = SV_COUT << "# [" << (progress ? completed++ : completed.load()) << "/" << total << "] "
                     << std::left << std::setw(maxLogLength + 10) << line;
    if (progress) (s << "\r").flush();
    else s << std::endl;
  }
};

} // namespace sv
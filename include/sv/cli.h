#pragma once

#include <iostream>

#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/CommandLine.h"

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

std::optional<llvm::Error> parseCategory(llvm::cl::OptionCategory &category, int &argc,
                                         const char **argv);

template <typename P, typename F>
static int parseAndRun(int argc, const char **argv, P parse, F run) {
  auto maybeOptions = parse(argc, argv);
  if (auto x = maybeOptions.takeError()) {
    SV_CERR << toString(std::move(x));
    return EXIT_FAILURE;
  }
  return run(maybeOptions.get());
}

class ProgressLogger {
  size_t total;
  int maxLogLength;
  std::atomic_size_t completed{1};

public:
  ProgressLogger(size_t total, int maxLogLength);
  void log(const std::string &line, bool progress = true);
};

} // namespace sv
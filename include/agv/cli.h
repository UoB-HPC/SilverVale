#pragma once

#include <iosfwd>

#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/CommandLine.h"

#ifdef __cpp_lib_syncbuf
  #include <syncstream>
  #define AGV_COUT std::osyncstream(std::cout)
#else
  #include <iostream>
  #define AGV_COUT std::cout
#endif

namespace agv {

std::optional<llvm::Error> parseCategory(llvm::cl::OptionCategory &category, int &argc,
                                         const char **argv);

template <typename P, typename F>
static int parseAndRun(int argc, const char **argv, P parse, F run) {
  auto maybeOptions = parse(argc, argv);
  if (auto x = maybeOptions.takeError()) {
    std::cerr << toString(std::move(x));
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

} // namespace agv
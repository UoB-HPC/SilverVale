#pragma once

#include <string>

namespace p3md::def {

struct Options {
  std::string output;
};

[[nodiscard]] llvm::Expected<def::Options> parse(int argc, const char **argv);

int run(const Options &options);

} // namespace p3md::def
#pragma once

#include "kind.h"
#include <string>
#include <vector>

namespace p3md::run {

struct Options {
  std::string script;
  std::vector<std::string> scriptRoots;
  int maxThreads;
  std::vector<std::string> args;
};

int run(const Options &options);

} // namespace p3md::run
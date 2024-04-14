#pragma once

#include <string>

namespace p3md::list {

enum class Kind { Entry, Dependencies };
struct Options {
  std::string dbDir;
  Kind kind;
};
int run(const Options &options);

} // namespace p3md::list
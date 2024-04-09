#pragma once

#include <string>

namespace p3md::list {

struct Options {
  std::string dbDir;
};
int run(const Options &options);

} // namespace p3md::list
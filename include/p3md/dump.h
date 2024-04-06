#pragma once

#include <string>
#include <vector>

namespace p3md::dump {

struct Options {
  std::string dbFile;
  std::vector<std::string> entries;
};
int run(const Options &options);

} // namespace p3md::dump
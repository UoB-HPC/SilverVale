#pragma once

#include <string>
#include <vector>

namespace p3md::diff {

struct Options {
  std::string leftDbFile;
  std::string rightDbFile;
  std::vector<std::string> entries;
};
int run(const Options &options);

} // namespace p3md::diff
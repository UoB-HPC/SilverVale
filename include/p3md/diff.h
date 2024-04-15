#pragma once

#include "kind.h"
#include <string>
#include <vector>

namespace p3md::diff {

struct Options {
  DataKind kind;
  std::vector<std::pair<std::string, std::vector<std::string>>> entries;
};
int run(const Options &lhsEntriesIdx);

} // namespace p3md::diff
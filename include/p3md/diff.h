#pragma once

#include "kind.h"
#include <string>
#include <vector>

namespace p3md::diff {

struct Options {
  std::vector<DataKind> kinds;
  std::vector<std::pair<std::string, std::vector<std::string>>> entries;
  std::vector<std::pair<std::vector<std::string>, std::string>> merges;
  std::vector<std::string> baseGlobs;
  std::vector<std::pair<std::string, std::string>> entryGlobPairs;
  std::string outputPrefix;
  int maxThreads;
};


// TODO
// --transform "include=*;exclude=*;merge=A:N;merge=B*:C" --match ""


int run(const Options &lhsEntriesIdx);

} // namespace p3md::diff
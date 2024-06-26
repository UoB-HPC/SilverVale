#pragma once

#include "kind.h"
#include <string>
#include <vector>

namespace p3md::diff {

struct CodeBase {
  std::string db;
  std::vector<std::string> roots;
};

struct EntryFilter {
  bool include;
  std::string glob;
};

struct EntryMerge {
  std::string glob;
  std::string name;
};

struct EntryMatch {
  std::string sourceGlob;
  std::string targetGlob;
};

struct Options {
  std::vector<DataKind> kinds;
  std::vector<CodeBase> codeBases;
  std::vector<std::variant<EntryFilter, EntryMerge>> transforms;
  std::vector<EntryMatch> matches;
  std::string outputPrefix;
  int maxThreads;
};

// TODO
// --transform "include=*;exclude=*;merge=A:N;merge=B*:C"
// --match ""

int run(const Options &lhsEntriesIdx);

} // namespace p3md::diff
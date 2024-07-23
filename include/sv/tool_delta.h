#pragma once

#include <string>
#include <variant>
#include <vector>

namespace sv::delta {

enum class Kind : uint8_t {
  SLOCRawAbs,
  LLOCRawAbs,
  SourceRawRel,
  TSTreeRawRel,

  SLOCAbs,
  LLOCAbs,
  SourceRel,
  TSTreeRel,

  STreeRel,
  STreeInlineRel,
  IRTreeRel
};

inline std::string_view to_string(const Kind &kind) {
  switch (kind) {
    case Kind::SLOCRawAbs: return "slocA";
    case Kind::LLOCRawAbs: return "llocA";
    case Kind::SourceRawRel: return "sourceR";
    case Kind::TSTreeRawRel: return "tstreeR";

    case Kind::SLOCAbs: return "slocA+p";
    case Kind::LLOCAbs: return "llocA+p";
    case Kind::SourceRel: return "sourceR+p";
    case Kind::TSTreeRel: return "tstreeR+p";

    case Kind::STreeRel: return "streeR";
    case Kind::STreeInlineRel: return "streeR+i";
    case Kind::IRTreeRel: return "irtreeR";
  }
  return "(unknown kind)";
}

struct DatabaseSpec {
  std::string path;
  std::vector<std::string> roots;
};

struct ExcludeFilter {
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
  std::vector<DatabaseSpec> databases;
  std::string base;
  std::vector<Kind> kinds;
  std::vector<ExcludeFilter> excludes;
  std::vector<EntryMerge> merges;
  std::vector<EntryMatch> matches;
  std::string outputPrefix;
  int maxThreads;
};

[[nodiscard]] int main(int argc, const char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace sv::delta
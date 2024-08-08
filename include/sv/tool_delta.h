#pragma once

#include <string>
#include <variant>
#include <vector>

namespace sv::delta {

constexpr const char *Name = "delta";
constexpr const char *Description = "Compare two or more SV databases";

enum class Kind : uint8_t {

  SrcLenAbs,
  SrcRel,
  SLOCAbs,
  LLOCAbs,
  TSTreeRel,

  STreeRel,
  STreeInlineRel,
  IRTreeRel,
};

static constexpr std::string_view to_string(const Kind &kind) {
  switch (kind) {
    case Kind::SrcLenAbs: return "srcLenA";
    case Kind::SrcRel: return "srcR";
    case Kind::SLOCAbs: return "slocA";
    case Kind::LLOCAbs: return "llocA";
    case Kind::TSTreeRel: return "tstreeR";
    case Kind::STreeRel: return "streeR";
    case Kind::STreeInlineRel: return "streeInlinedR";
    case Kind::IRTreeRel: return "irtreeR";
  }
  return "(unknown kind)";
}

static constexpr std::optional<Kind> parseKind(const std::string_view &kind) {
  if (kind == "srclen" || kind == "srcLenA") return Kind::SrcLenAbs;
  if (kind == "src" || kind == "srcR") return Kind::SrcRel;
  if (kind == "sloc" || kind == "slocA") return Kind::SLOCAbs;
  if (kind == "lloc" || kind == "llocA") return Kind::LLOCAbs;
  if (kind == "tstree" || kind == "tstreeR") return Kind::TSTreeRel;
  if (kind == "stree" || kind == "streeR") return Kind::STreeRel;
  if (kind == "streeinlined" || kind == "streeInlinedR") return Kind::STreeInlineRel;
  if (kind == "irtree" || kind == "irtreeR") return Kind::IRTreeRel;
  return std::nullopt;
}

enum class Modifier : uint8_t { Raw, CPP, Cov };

static constexpr std::string_view to_string(const Modifier &modifier) {
  switch (modifier) {
    case Modifier::Raw: return "Raw";
    case Modifier::CPP: return "CPP";
    case Modifier::Cov: return "Cov";
  }
  return "(unknown modifier)";
}

static std::optional<Modifier> parseModifier(const std::string_view &modifier) {
  if (modifier == "raw") return Modifier::Raw;
  if (modifier == "cpp") return Modifier::CPP;
  if (modifier == "cov") return Modifier::Cov;
  return std::nullopt;
}

struct DatabaseSpec {
  bool base;
  std::filesystem::path path;
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

struct TaskDesc {
  delta::Kind kind;
  delta::Modifier mod;
};

struct Options {
  std::vector<DatabaseSpec> databases;
  std::vector<TaskDesc> kinds;
  std::vector<ExcludeFilter> excludes;
  std::vector<EntryMerge> merges;
  std::vector<EntryMatch> matches;
  std::string outputPrefix;
  int maxThreads;
};

[[nodiscard]] int main(int argc, char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace sv::delta
#pragma once

#include "database.h"
#include <string>
#include <vector>

namespace sv::index {

constexpr const char *Name = "index";
constexpr const char *Description = "Build SV database from compile_commands.json";

enum class CoverageKind : uint8_t { AutoDetect, ClangSBCC, GCov };

static std::optional<CoverageKind> parseCoverageKind(const std::string_view &kind) {
  if (kind == "auto") return CoverageKind::AutoDetect;
  if (kind == "gcov") return CoverageKind::GCov;
  if (kind == "sbcc") return CoverageKind::ClangSBCC;
  return std::nullopt;
}

struct Options {
  std::filesystem::path buildDir;
  std::vector<std::string> includeGlobs;
  std::vector<std::string> excludeGlobs;
  std::filesystem::path outDir;
  std::filesystem::path coverageBin;
  std::filesystem::path coverageRawDir;
  CoverageKind coverageKind;

  bool clearOutDir;
  bool verbose;
  int maxThreads;

  [[nodiscard]] std::unique_ptr<sv::CompilationDatabase> resolveDatabase() const;
};

[[nodiscard]] int main(int argc, char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace sv::index
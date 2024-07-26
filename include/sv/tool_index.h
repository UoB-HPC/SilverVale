#pragma once

#include "database.h"
#include <string>
#include <vector>

namespace sv::index {

enum class CoverageKind : uint8_t { AutoDetect, ClangSBCC, GCov };

struct Options {
  std::filesystem::path buildDir;
  std::vector<std::string> sourceGlobs;
  std::filesystem::path outDir;
  std::filesystem::path coverageBin;
  std::filesystem::path coverageRawDir;
  CoverageKind coverageKind;

  bool clearOutDir;
  bool verbose;
  int maxThreads;

  [[nodiscard]] std::unique_ptr<sv::CompilationDatabase> resolveDatabase() const;
};

[[nodiscard]] int main(int argc, const char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace sv::index
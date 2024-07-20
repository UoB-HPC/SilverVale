#pragma once

#include <string>
#include <vector>

#include <clang/Tooling/ArgumentsAdjusters.h>
#include <clang/Tooling/CompilationDatabase.h>

namespace agv::index {

struct Options {
  std::string buildDir;
  std::vector<std::string> sourceGlobs;
  std::vector<std::string> argsBefore;
  std::vector<std::string> argsAfter;
  std::string outDir;
  bool clearOutDir;
  bool verbose;
  bool noCompress;
  int maxThreads;

  [[nodiscard]] clang::tooling::ArgumentsAdjuster resolveAdjuster() const;
  [[nodiscard]] std::unique_ptr<clang::tooling::CompilationDatabase>
  resolveDatabase(const clang::tooling::ArgumentsAdjuster &adjuster) const;
};

[[nodiscard]] int main(int argc, const char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace agv::index
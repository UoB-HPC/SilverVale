#pragma once

#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

namespace p3md {

struct BuildOptions {

  std::string BuildDir;
  std::vector<std::string> Roots;
  std::string CompilationDBFile;
  std::unique_ptr<clang::tooling::CompilationDatabase> Compilations;
  std::vector<std::string> SourcePathList;
  clang::tooling::ArgumentsAdjuster Adjuster;

  [[nodiscard]] static llvm::Expected<BuildOptions> parse(int &argc, const char **argv);

  [[nodiscard]] std::optional<std::string> resolve(const std::string &sourcePath) const;
};

struct ListOptions {
  std::string dbFile;
  [[nodiscard]] static llvm::Expected<ListOptions> parse(int &argc, const char **argv);
};

struct DiffOptions {
  std::string leftDbFile;
  std::string rightDbFile;
  std::vector<std::string> entries;
  [[nodiscard]] static llvm::Expected<DiffOptions> parse(int &argc, const char **argv);
};

struct DumpOptions {
  std::string dbFile;
  std::vector<std::string> entries;
  [[nodiscard]] static llvm::Expected<DumpOptions> parse(int &argc, const char **argv);
};

} // namespace p3md

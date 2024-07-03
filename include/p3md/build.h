#pragma once

#include <ctime>
#include <string>
#include <unordered_map>
#include <vector>

#include "clang/AST/Decl.h"
#include "clang/Frontend/ASTUnit.h"
#include <clang/Tooling/ArgumentsAdjusters.h>
#include <clang/Tooling/CompilationDatabase.h>

#include "json.hpp"
#include "p3md/tree.h"

namespace p3md::build {

struct Options {

  std::string buildDir;
  std::vector<std::string> sourceGlobs;
  std::vector<std::string> argsBefore;
  std::vector<std::string> argsAfter;
  std::optional<std::string> clangResourceDir;
  std::string outDir;
  bool clearOutDir;
  bool verbose;
  bool compress;
  int maxThreads;

  [[nodiscard]] clang::tooling::ArgumentsAdjuster resolveAdjuster() const;
  [[nodiscard]] std::unique_ptr<clang::tooling::CompilationDatabase>
  resolveDatabase(const clang::tooling::ArgumentsAdjuster &adjuster) const;
};

int run(const Options &options);

} // namespace p3md::build
#pragma once

#include <string>
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
  std::vector<std::string> rootDirs;
  //  std::unique_ptr<clang::tooling::CompilationDatabase> compilation;
  std::string sourceGlob;
  std::vector<std::string> argsBefore;
  std::vector<std::string> argsAfter;
  std::string outDir;
  bool clearOutDir;
  int maxThreads;

  [[nodiscard]] std::optional<std::string> resolve(const std::string &sourcePath) const;
  [[nodiscard]] clang::tooling::ArgumentsAdjuster resolveAdjuster() const;
  [[nodiscard]] std::unique_ptr<clang::tooling::CompilationDatabase>
  resolveDatabase(clang::tooling::ArgumentsAdjuster adjuster) const;
};

struct Database {
  struct Entry {
    std::string rootRelativeFile;
    std::string compileLine;
    std::string source;
    std::string pchName;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, rootRelativeFile, compileLine, source, pchName);
  };
  size_t clangMajorVersion, clangMinorVersion, clangPatchVersion;
  std::vector<std::string> roots;
  std::vector<Entry> entries;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(Database, roots, entries);
};

std::vector<clang::Decl *> topLevelDeclsInMainFile(clang::ASTUnit &unit);

int run(const Options &options);

} // namespace p3md::build
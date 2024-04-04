#pragma once

#include "clang/AST/Decl.h"
#include "clang/Frontend/ASTUnit.h"

#include "json.hpp"
#include "p3md/tree.h"

namespace p3md {

struct Database {
  struct Entry {
    std::string rootRelativeFile;
    std::string compileLine;
    std::string source;
    std::vector<std::byte> astData;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, rootRelativeFile, compileLine, source, astData);
  };
  explicit Database(clang::ASTUnit &unit);
  std::vector<std::string> roots;
  std::vector<Entry> entries;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(Database, roots, entries);
};

enum class DataKind : uint8_t { Source = 1, STree, TSNode };

std::vector<clang::Decl *> topLevelDeclsInMainFile(clang::ASTUnit &unit);

int build_main(int argc, const char **argv);
int list_main(int argc, const char **argv);
int diff_main(p3md::DataKind kind,int argc, const char **argv);
int dump_main(p3md::DataKind kind,int argc, const char **argv);

} // namespace p3md
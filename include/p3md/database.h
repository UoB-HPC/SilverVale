#pragma once

#include <ctime>
#include <memory>
#include <string>
#include <map>
#include <vector>

#include "clang/Frontend/ASTUnit.h"

#include "json.hpp"

namespace p3md {

struct Database {
  struct Source {
    std::time_t modified{};
    std::string content;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Source, modified, content);
  };
  struct PCHEntry {
    std::vector<std::string> compileCommands;
    std::string pchName;
    std::string pchHash;
    std::map<std::string, std::string> dependencies;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(PCHEntry, compileCommands, pchName, pchHash, dependencies);
  };
  size_t clangMajorVersion{}, clangMinorVersion{}, clangPatchVersion{};
  std::map<std::string, PCHEntry> entries{};
  std::map<std::string, Source> dependencies{};
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(Database, //
                                 clangMajorVersion, clangMinorVersion, clangPatchVersion, entries,
                                 dependencies);

  static std::unique_ptr<Database> fromJson(const std::string &json);

  class Materialised {
    std::vector<std::vector<char>> astBackingBuffer;

  public:
    explicit Materialised(const Database &db, const std::string &baseDir);
    std::map<std::string, std::unique_ptr<clang::ASTUnit>> units;
  };
};

} // namespace p3md

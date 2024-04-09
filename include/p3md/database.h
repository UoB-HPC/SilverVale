#pragma once

#include <ctime>
#include <string>
#include <unordered_map>
#include <vector>

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
    std::unordered_map<std::string, std::string> dependencies;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(PCHEntry, compileCommands, pchName, pchHash, dependencies);
  };
  size_t clangMajorVersion{}, clangMinorVersion{}, clangPatchVersion{};
  std::unordered_map<std::string, PCHEntry> entries;
  std::unordered_map<std::string, Source> dependencies;
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(Database, clangMajorVersion, clangMinorVersion, clangPatchVersion,
                                 entries, dependencies);
};

} // namespace p3md

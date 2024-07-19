#pragma once

#include <iosfwd>
#include <map>
#include <optional>
#include <string>

#include "agv/lua_shim.h"
#include "json.hpp"

namespace agv {

struct Database {

  struct Dependency {
    std::time_t modified{};
    std::string content{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Dependency, modified, content);
    DEF_TEAL_SOL_UT(Dependency,                      //
                    SOL_UT_RO(Dependency, modified), //
                    SOL_UT_RO(Dependency, content));
  };

  struct Bitcode {
    std::string name{};
    std::string kind{};
    std::string triple{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Bitcode, name, kind, triple);
    DEF_TEAL_SOL_UT(Bitcode,                  //
                    SOL_UT_RO(Bitcode, name), //
                    SOL_UT_RO(Bitcode, kind), //
                    SOL_UT_RO(Bitcode, triple));
    friend std::ostream &operator<<(std::ostream &os, const Database::Bitcode &bitcode) {
      return os << "agv::Database::Bitcode{"        //
                << ".name=" << bitcode.name << ", " //
                << ".kind=" << bitcode.kind << ", " //
                << ".triple=" << bitcode.triple     //
                << "}";
    }
  };

  struct Entry {
    std::string compileCommand{};
    std::string pchName{};
    std::vector<Bitcode> bitcodes{};
    std::map<std::string, std::string> dependencies{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, compileCommand, pchName, bitcodes, dependencies);

  private:
    DEF_SOL_UT_ACCESSOR(compileCommand)
    DEF_SOL_UT_ACCESSOR(pchName)
    DEF_SOL_UT_ACCESSOR(bitcodes)
    DEF_SOL_UT_ACCESSOR(dependencies)
  public:
    DEF_TEAL_SOL_UT(Entry,                                //
                    SOL_UT_FN_ACC(Entry, compileCommand), //
                    SOL_UT_FN_ACC(Entry, pchName),        //
                    SOL_UT_FN_ACC(Entry, bitcodes),       //
                    SOL_UT_FN_ACC(Entry, dependencies));

    friend std::ostream &operator<<(std::ostream &os, const Database::Entry &entry) {
      os << "agv::Database::Entry{"                                        //
         << ".pchName=" << entry.pchName << ", "                           //
         << ".compileCommands=" << entry.compileCommand << ", "            //
         << ".dependencies=(" << entry.dependencies.size() << ")" << ", "; //
      for (size_t i = 0; i < entry.bitcodes.size(); ++i)                   //
        os << ".bitcodes[" << i << "]=" << entry.bitcodes[i] << ", ";      //
      os << "}";
      return os;
    }
  };

  std::map<std::string, std::string> attributes{};
  std::string root{};
  std::map<std::string, Entry> entries{};
  std::map<std::string, Dependency> dependencies{};

  // FIXME move to Codebase and add FlatDB versions
  [[nodiscard]] static Database fromJsonString(const std::string &json);
  [[nodiscard]] static Database fromJsonStream(std::ifstream &stream);
  [[nodiscard]] static Database fromJsonFile(const std::string &file);

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(Database,   //
                                 attributes, //
                                 root, entries, dependencies);

private:
  DEF_SOL_UT_ACCESSOR(attributes)
  DEF_SOL_UT_ACCESSOR(root)
  DEF_SOL_UT_ACCESSOR(entries)
  DEF_SOL_UT_ACCESSOR(dependencies)

public:
  DEF_TEAL_SOL_UT(Database,                              //
                  SOL_UT_FN_ACC(Database, attributes),   //
                  SOL_UT_FN_ACC(Database, root),         //
                  SOL_UT_FN_ACC(Database, entries),      //
                  SOL_UT_FN_ACC(Database, dependencies), //
                  SOL_UT_FN(Database, fromJsonString),   //
                  SOL_UT_FN(Database, fromJsonFile));

  friend std::ostream &operator<<(std::ostream &os, const Database &database) {
    os << "agv::Database{"; //
    for (auto &[k, v] : database.attributes)
      os << ".attributes[" << k << "]=" << v << ", ";
    for (auto &[k, v] : database.entries)
      os << ".entries[" << k << "]=" << v << ", ";
    os << ".dependencies=(" << database.dependencies.size() << ")" //
       << "}";
    return os;
  }
};

struct FlatDatabase {

  struct Entry {
    std::string filename;
    std::string command;
    std::string raw{};
    std::string preprocessed{};

    std::string namedSTreeFile{};
    std::string unnamedSTreeFile{};
    std::string namedIRTreeFile{};
    std::string unnamedIRTreeFile{};

    std::map<std::string, std::string> attributes{};

    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry,                                //
                                   filename, command, raw, preprocessed, //
                                   namedIRTreeFile, unnamedSTreeFile,    //
                                   namedIRTreeFile, unnamedIRTreeFile,   //
                                   attributes);

  private:
    DEF_SOL_UT_ACCESSOR(filename);
    DEF_SOL_UT_ACCESSOR(command);
    DEF_SOL_UT_ACCESSOR(raw);
    DEF_SOL_UT_ACCESSOR(preprocessed);
    DEF_SOL_UT_ACCESSOR(namedSTreeFile);
    DEF_SOL_UT_ACCESSOR(unnamedSTreeFile);
    DEF_SOL_UT_ACCESSOR(namedIRTreeFile);
    DEF_SOL_UT_ACCESSOR(unnamedIRTreeFile);
    DEF_SOL_UT_ACCESSOR(attributes);

  public:
    DEF_TEAL_SOL_UT(Entry,                                   //
                    SOL_UT_FN_ACC(Entry, filename),          //
                    SOL_UT_FN_ACC(Entry, command),           //
                    SOL_UT_FN_ACC(Entry, raw),               //
                    SOL_UT_FN_ACC(Entry, preprocessed),      //
                    SOL_UT_FN_ACC(Entry, namedSTreeFile),    //
                    SOL_UT_FN_ACC(Entry, unnamedSTreeFile),  //
                    SOL_UT_FN_ACC(Entry, namedIRTreeFile),   //
                    SOL_UT_FN_ACC(Entry, unnamedIRTreeFile), //
                    SOL_UT_FN_ACC(Entry, attributes));

    friend std::ostream &operator<<(std::ostream &os, const Entry &database) {
      os << "agv::FlatDatabase::Entry{"                                  //
         << ".filename=" << database.filename << ", "                    //
         << ".command=" << database.command << ", "                      //
         << ".raw=" << database.raw << ", "                              //
         << ".preprocessed=" << database.preprocessed << ", "            //
         << ".namedSTreeFile=" << database.namedSTreeFile << ", "        //
         << ".unnamedSTreeFile=" << database.unnamedSTreeFile << ", "    //
         << ".namedIRTreeFile=" << database.namedIRTreeFile << ", "      //
         << ".unnamedIRTreeFile=" << database.unnamedIRTreeFile << ", "; //
      for (auto &[k, v] : database.attributes)
        os << ".attributes[" << k << "]=" << v << ", ";
      os << "}";
      return os;
    }
  };
};

} // namespace agv

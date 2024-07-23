#pragma once

#include <iosfwd>
#include <map>
#include <optional>
#include <string>

#include "agv/lua_shim.h"
#include "nlohmann/json.hpp"

namespace agv {

struct CompilationDatabase {
  struct Entry {
    std::string directory;
    std::vector<std::string> command;
    std::string file;
    std::string output;
    NLOHMANN_DEFINE_TYPE_INTRUSIVE_ONLY_SERIALIZE(Entry, directory, command, file, output);

    friend void from_json(const nlohmann::json &json, Entry &entry) {
      // see https://clang.llvm.org/docs/JSONCompilationDatabase.html
      json.at("directory").get_to(entry.directory);
      json.at("file").get_to(entry.file);
      json.at("output").get_to(entry.output);
      // we may see either arguments: string[] or command: string
      if (json.contains("command")) {
        auto cmd = json.at("command").get<std::string>();
        std::istringstream iss(cmd);
        std::string s;
        while (getline(iss, s, ' '))
          entry.command.emplace_back(s);
      } else if (json.contains("arguments")) json.at("arguments").get_to(entry.command);
    }

  private:
    DEF_SOL_UT_ACCESSOR(directory);
    DEF_SOL_UT_ACCESSOR(command);
    DEF_SOL_UT_ACCESSOR(file);
    DEF_SOL_UT_ACCESSOR(output);

  public:
    DEF_TEAL_SOL_UT(Entry,                           //
                    SOL_UT_FN_ACC(Entry, directory), //
                    SOL_UT_FN_ACC(Entry, command),   //
                    SOL_UT_FN_ACC(Entry, file),      //
                    SOL_UT_FN_ACC(Entry, output));
  };

  std::vector<Entry> entries;
  friend void to_json(nlohmann::json &json, const CompilationDatabase &db) { json = db.entries; }
  friend void from_json(const nlohmann::json &json, CompilationDatabase &db) {
    json.get_to(db.entries);
  }

private:
  DEF_SOL_UT_ACCESSOR(entries);

public:
  DEF_TEAL_SOL_UT(CompilationDatabase, //
                  SOL_UT_FN_ACC(CompilationDatabase, entries));
};

struct Dependency {
  std::time_t modified{};
  std::string content{};
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(Dependency, modified, content);

private:
  DEF_SOL_UT_ACCESSOR(modified);
  DEF_SOL_UT_ACCESSOR(content);

public:
  DEF_TEAL_SOL_UT(Dependency,                          //
                  SOL_UT_FN_ACC(Dependency, modified), //
                  SOL_UT_FN_ACC(Dependency, content));
};

struct LLVMBitcode {
  std::string file{};
  std::string kind{};
  std::string triple{};
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(LLVMBitcode, file, kind, triple);

private:
  DEF_SOL_UT_ACCESSOR(file);
  DEF_SOL_UT_ACCESSOR(kind);
  DEF_SOL_UT_ACCESSOR(triple);

public:
  DEF_TEAL_SOL_UT(LLVMBitcode,                      //
                  SOL_UT_FN_ACC(LLVMBitcode, file), //
                  SOL_UT_FN_ACC(LLVMBitcode, kind), //
                  SOL_UT_FN_ACC(LLVMBitcode, triple));
  friend std::ostream &operator<<(std::ostream &os, const LLVMBitcode &bitcode) {
    return os << "agv::LLVMBitcode{"              //
              << ".file=" << bitcode.file << ", " //
              << ".kind=" << bitcode.kind << ", " //
              << ".triple=" << bitcode.triple     //
              << "}";
  }
};

struct ClangEntry {

  std::string kind = "clang";
  std::string language;

  std::string file;
  std::string command;
  std::string preprocessed{};

  std::string pchFile{};
  std::vector<LLVMBitcode> bitcodes{};
  std::map<std::string, Dependency> dependencies{};
  std::map<std::string, std::string> attributes{};

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(ClangEntry, kind, language, file, command, preprocessed, //
                                 pchFile, bitcodes, dependencies, attributes);

private:
  DEF_SOL_UT_ACCESSOR(kind);
  DEF_SOL_UT_ACCESSOR(language);
  DEF_SOL_UT_ACCESSOR(file);
  DEF_SOL_UT_ACCESSOR(command);
  DEF_SOL_UT_ACCESSOR(preprocessed);
  DEF_SOL_UT_ACCESSOR(pchFile);
  DEF_SOL_UT_ACCESSOR(bitcodes);
  DEF_SOL_UT_ACCESSOR(dependencies);
  DEF_SOL_UT_ACCESSOR(attributes);

public:
  DEF_TEAL_SOL_UT(ClangEntry,                              //
                  SOL_UT_FN_ACC(ClangEntry, kind),         //
                  SOL_UT_FN_ACC(ClangEntry, language),     //
                  SOL_UT_FN_ACC(ClangEntry, file),         //
                  SOL_UT_FN_ACC(ClangEntry, command),      //
                  SOL_UT_FN_ACC(ClangEntry, preprocessed), //
                  SOL_UT_FN_ACC(ClangEntry, pchFile),      //
                  SOL_UT_FN_ACC(ClangEntry, bitcodes),     //
                  SOL_UT_FN_ACC(ClangEntry, dependencies), //
                  SOL_UT_FN_ACC(ClangEntry, attributes));

  friend std::ostream &operator<<(std::ostream &os, const ClangEntry &entry) {
    os << "agv::ClangEntry{"                                        //
       << ".kind=" << entry.kind << ", "                            //
       << ".language=" << entry.language << ", "                    //
       << ".file=" << entry.file << ", "                            //
       << ".commands=" << entry.command << ", "                     //
       << ".preprocessed=(" << entry.preprocessed.size() << "), "   //
       << ".pchFile=" << entry.pchFile << ", ";                     //
    for (size_t i = 0; i < entry.bitcodes.size(); ++i)              //
      os << ".bitcodes[" << i << "]=" << entry.bitcodes[i] << ", "; //
    for (auto &[k, v] : entry.dependencies)
      os << ".dependencies[" << k << "]=(" << v.content.size() << "), ";
    for (auto &[k, v] : entry.attributes)
      os << ".attributes[" << k << "]=" << v << ", ";
    os << "}";
    return os;
  }
};

struct FlatEntry {

  std::string kind = "flat";
  std::string language;

  std::string file;
  std::string command;
  std::string preprocessed{};

  std::string namedSTreeFile{};
  std::string unnamedSTreeFile{};
  std::string namedIRTreeFile{};
  std::string unnamedIRTreeFile{};

  std::map<std::string, Dependency> dependencies{};
  std::map<std::string, std::string> attributes{};

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(FlatEntry,                                   //
                                 kind, language, file, command, preprocessed, //
                                 namedSTreeFile, unnamedSTreeFile,            //
                                 namedIRTreeFile, unnamedIRTreeFile,          //
                                 dependencies, attributes);

private:
  DEF_SOL_UT_ACCESSOR(kind);
  DEF_SOL_UT_ACCESSOR(language);
  DEF_SOL_UT_ACCESSOR(file);
  DEF_SOL_UT_ACCESSOR(command);
  DEF_SOL_UT_ACCESSOR(preprocessed);
  DEF_SOL_UT_ACCESSOR(namedSTreeFile);
  DEF_SOL_UT_ACCESSOR(unnamedSTreeFile);
  DEF_SOL_UT_ACCESSOR(namedIRTreeFile);
  DEF_SOL_UT_ACCESSOR(unnamedIRTreeFile);
  DEF_SOL_UT_ACCESSOR(dependencies);
  DEF_SOL_UT_ACCESSOR(attributes);

public:
  DEF_TEAL_SOL_UT(FlatEntry,                                   //
                  SOL_UT_FN_ACC(FlatEntry, kind),              //
                  SOL_UT_FN_ACC(FlatEntry, language),          //
                  SOL_UT_FN_ACC(FlatEntry, file),              //
                  SOL_UT_FN_ACC(FlatEntry, command),           //
                  SOL_UT_FN_ACC(FlatEntry, preprocessed),      //
                  SOL_UT_FN_ACC(FlatEntry, namedSTreeFile),    //
                  SOL_UT_FN_ACC(FlatEntry, unnamedSTreeFile),  //
                  SOL_UT_FN_ACC(FlatEntry, namedIRTreeFile),   //
                  SOL_UT_FN_ACC(FlatEntry, unnamedIRTreeFile), //
                  SOL_UT_FN_ACC(FlatEntry, dependencies),      //
                  SOL_UT_FN_ACC(FlatEntry, attributes));

  friend std::ostream &operator<<(std::ostream &os, const FlatEntry &db) {
    os << "agv::FlatEntry{"                                      //
       << ".kind=" << db.kind << ", "                            //
       << ".language=" << db.language << ", "                    //
       << ".file=" << db.file << ", "                            //
       << ".command=" << db.command << ", "                      //
       << ".preprocessed=(" << db.preprocessed.size() << "), "   //
       << ".namedSTreeFile=" << db.namedSTreeFile << ", "        //
       << ".unnamedSTreeFile=" << db.unnamedSTreeFile << ", "    //
       << ".namedIRTreeFile=" << db.namedIRTreeFile << ", "      //
       << ".unnamedIRTreeFile=" << db.unnamedIRTreeFile << ", "; //
    for (auto &[k, v] : db.dependencies)
      os << ".dependencies[" << k << "]=(" << v.content.size() << "), ";
    for (auto &[k, v] : db.attributes)
      os << ".attributes[" << k << "]=" << v << ", ";
    os << "}";
    return os;
  }
};

struct Database {
  std::string root{};
  std::vector<std::variant<ClangEntry, FlatEntry>> entries{};

private:
  DEF_SOL_UT_ACCESSOR(root);
  DEF_SOL_UT_ACCESSOR(entries);

public:
  DEF_TEAL_SOL_UT(Database,                      //
                  SOL_UT_FN_ACC(Database, root), //
                  SOL_UT_FN_ACC(Database, entries));

  friend std::ostream &operator<<(std::ostream &os, const Database &db) {
    os << "agv::Database{"             //
       << ".root=" << db.root << ", "; //
    for (auto &e : db.entries)
      std::visit([&](auto &&e) { os << ".entries[" << e.file << "]=" << e << ", "; }, e);
    os << "}";
    return os;
  }
};

} // namespace agv

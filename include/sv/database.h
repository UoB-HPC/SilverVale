#pragma once

#include <iosfwd>
#include <map>
#include <optional>
#include <string>
#include <variant>

#include "nlohmann/json.hpp"
#include "sv/lua_shim.h"

namespace sv {

constexpr std::string_view EntrySuffix = "sv.json";
constexpr std::string_view EntryClangSBCCName = "coverage.clang_sbcc.sv.json";
constexpr std::string_view EntryGCCGCovName = "coverage.gcc_gcov.sv.json";

struct GCCGCovProfile {

  struct Line {
    size_t line_number;
    std::string function_name;
    size_t count;
    bool unexecuted_block;
    // XXX `branches` not implemented

    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Line, line_number, function_name, count, unexecuted_block);
  };

  struct Function {
    std::string name{};
    std::string demangled_name{};
    size_t start_line{};
    size_t start_column{};
    size_t end_line{};
    size_t end_column{};
    size_t blocks{};
    size_t blocks_executed{};
    size_t execution_count{};

    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Function, name, demangled_name, start_line, start_column,
                                   end_line, end_column, blocks, blocks_executed, execution_count);
  };

  struct Entry {
    std::string file{};
    std::vector<Function> functions{};
    std::vector<Line> lines{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, file, functions);
  };

  std::string format_version{};
  std::string gcc_version{};
  std::string current_working_directory{};
  std::string data_file{};
  std::vector<Entry> files{};

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(GCCGCovProfile, format_version, gcc_version,
                                 current_working_directory, data_file, files);
};

struct ClangSBCCProfile {

  struct Region {
    size_t LineStart, ColumnStart, LineEnd, ColumnEnd, //
        ExecutionCount,                                //
        FileID, ExpandedFileID, Kind;
    friend void from_json(const nlohmann::json &json, Region &entry) {
      json.at(0).get_to(entry.LineStart);
      json.at(1).get_to(entry.ColumnStart);
      json.at(2).get_to(entry.LineEnd);
      json.at(3).get_to(entry.ColumnEnd);
      json.at(4).get_to(entry.ExecutionCount);
      json.at(5).get_to(entry.FileID);
      json.at(6).get_to(entry.ExpandedFileID);
      json.at(7).get_to(entry.Kind);
    }
    friend void to_json(nlohmann::json &json, const Region &entry) {
      json = std::vector{
          entry.LineStart,      entry.ColumnStart, entry.LineEnd,        entry.ColumnEnd,
          entry.ExecutionCount, entry.FileID,      entry.ExpandedFileID, entry.Kind};
    }
  };

  struct Branch {
    size_t LineStart, ColumnStart, LineEnd, ColumnEnd, //
        ExecutionCount, FalseExecutionCount,           //
        FileID, ExpandedFileID, Kind;                  //
    friend void from_json(const nlohmann::json &json, Branch &entry) {
      json.at(0).get_to(entry.LineStart);
      json.at(1).get_to(entry.ColumnStart);
      json.at(2).get_to(entry.LineEnd);
      json.at(3).get_to(entry.ColumnEnd);
      json.at(4).get_to(entry.ExecutionCount);
      json.at(5).get_to(entry.FalseExecutionCount);
      json.at(6).get_to(entry.FileID);
      json.at(7).get_to(entry.ExpandedFileID);
      json.at(8).get_to(entry.Kind);
    }
    friend void to_json(nlohmann::json &json, const Branch &entry) {
      json = std::vector{entry.LineStart, entry.ColumnStart,    entry.LineEnd,
                         entry.ColumnEnd, entry.ExecutionCount, entry.FalseExecutionCount,
                         entry.FileID,    entry.ExpandedFileID, entry.Kind};
    }
  };

  struct Function {
    std::string name{};
    std::vector<std::string> filenames{};
    std::vector<Region> regions{};
    std::vector<Branch> branches{};
    size_t count{};

    // XXX `mdmc_records` not implemented
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Function, name, filenames, regions, branches, count);
  };

  struct Entry {
    // XXX `files` (expansions) not implemented
    std::vector<Function> functions{};
    std::map<std::string, std::map<std::string, int>> totals{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, functions, totals);
  };

  std::string type{};
  std::string version{};
  std::vector<Entry> data{};

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(ClangSBCCProfile, type, version, data);
};

struct CompilationDatabase {
  struct Entry {
    std::string directory;
    std::vector<std::string> command;
    std::string file;
    std::string output;

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
    return os << "sv::LLVMBitcode{"               //
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
    os << "sv::ClangEntry{"                                         //
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
    os << "sv::FlatEntry{"                                       //
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
    os << "sv::Database{"              //
       << ".root=" << db.root << ", "; //
    for (auto &e : db.entries)
      std::visit([&](auto &&e) { os << ".entries[" << e.file << "]=" << e << ", "; }, e);
    os << "}";
    return os;
  }
};

} // namespace sv

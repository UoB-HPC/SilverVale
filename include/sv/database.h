#pragma once

#include <exception>
#include <iostream>
#include <map>
#include <optional>
#include <string>
#include <variant>

#include "nlohmann/json.hpp"
#include "sv/lua_shim.h"

#include "bxzstr.hpp"

namespace sv {

template <typename T> void writePacked(const std::string &path, const T &t) {
  try {
    //   std::fstream out(path, std::ios::out | std::ios::binary | std::ios::trunc);
    //  out << nlohmann::json(t);
    bxz::ofstream out(path, bxz::zstd, 4);
    out.exceptions(std::ios::badbit | std::ios::failbit);
    nlohmann::json::to_msgpack(nlohmann::json(t), out);
  } catch (const std::exception &e) {
    throw std::runtime_error("Cannot write file: " + path + ": " + e.what());
  }
}

template <typename T> T readPacked(const std::string &path, T &t) {
  try {
    //  std::fstream in(path, std::ios::in | std::ios::binary);
    //  nlohmann::from_json(nlohmann::json::parse(in), t);
    bxz::ifstream in(path);
    in.exceptions(std::ios::badbit | std::ios::failbit);
    nlohmann::from_json(nlohmann::json::from_msgpack(in), t);
    return t;
  } catch (const std::exception &e) {
    throw std::runtime_error("Cannot parse file: " + path + ": " + e.what());
  }
}

template <typename T> T readPacked(const std::string &path) {
  T t{};
  return readPacked<T>(path, t);
}

constexpr std::string_view EntrySuffix = "sv.json";
constexpr std::string_view EntryDepSuffix = "dep.json";

constexpr std::string_view EntryClangSBCCName = "coverage.clang_sbcc.sv.json";
constexpr std::string_view EntryGCCGCovName = "coverage.gcc_gcov.sv.json";

constexpr std::string_view EntryNamedSTreeSuffix = "named.stree.json";
constexpr std::string_view EntryNamedSTreeInlinedSuffix = "named.streeinlined.json";
constexpr std::string_view EntryNamedIrTreeSuffix = "named.irtree.json";

constexpr std::string_view EntryUnnamedSTreeSuffix = "unnamed.stree.json";
constexpr std::string_view EntryUnnamedSTreeInlinedSuffix = "unnamed.streeinlined.json";
constexpr std::string_view EntryUnnamedIrTreeSuffix = "unnamed.irtree.json";

struct GCCGCovProfile {

  struct Line {
    size_t line_number{};
    // XXX not all lines have a `function_name`
    size_t count{};
    bool unexecuted_block{};
    // XXX `branches` not implemented
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Line, line_number, count, unexecuted_block);
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
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, file, functions, lines);
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

  // Field names matches spec: https://gcc.gnu.org/onlinedocs/gcc/Invoking-Gcov.html
  struct Function {
    std::string name{};
    std::vector<std::string> filenames{};
    std::vector<Region> regions{};
    std::vector<Branch> branches{};
    size_t count{};

    // XXX `mdmc_records` not implemented
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Function, name, filenames, regions, branches, count);
  };

  // Field names matches spec: https://gcc.gnu.org/onlinedocs/gcc/Invoking-Gcov.html
  struct Entry {
    // XXX `files` (expansions) not implemented
    std::vector<Function> functions{};
    std::map<std::string, std::map<std::string, int>> totals{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, functions, totals);
  };

  // Field names matches spec: https://gcc.gnu.org/onlinedocs/gcc/Invoking-Gcov.html
  std::string type{};
  std::string version{};
  std::vector<Entry> data{};

  NLOHMANN_DEFINE_TYPE_INTRUSIVE(ClangSBCCProfile, type, version, data);
};

struct PerFileCoverage {
  struct Instance {
    std::string function{};
    size_t lineStart{}, lineEnd{};
    size_t colStart{}, colEnd{};
    size_t count{};

  private:
    DEF_SOL_UT_ACCESSOR(function);
    DEF_SOL_UT_ACCESSOR(lineStart);
    DEF_SOL_UT_ACCESSOR(lineEnd);
    DEF_SOL_UT_ACCESSOR(colStart);
    DEF_SOL_UT_ACCESSOR(colEnd);
    DEF_SOL_UT_ACCESSOR(count);

  public:
    DEF_TEAL_SOL_UT(Instance,                           //
                    SOL_UT_FN_ACC(Instance, function),  //
                    SOL_UT_FN_ACC(Instance, lineStart), //
                    SOL_UT_FN_ACC(Instance, lineEnd),   //
                    SOL_UT_FN_ACC(Instance, colStart),  //
                    SOL_UT_FN_ACC(Instance, colEnd),    //
                    SOL_UT_FN_ACC(Instance, count));
  };

  std::map<std::string, std::vector<Instance>> instances{};

private:
  DEF_SOL_UT_ACCESSOR(instances);

public:
  DEF_TEAL_SOL_UT(PerFileCoverage, SOL_UT_FN_ACC(PerFileCoverage, instances));
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

struct Database {

  struct Entry {

    std::string language;
    std::string path;
    std::string command;
    std::string preprocessedFile{};
    std::string dependencyFile{};
    std::vector<std::string> treeFiles;
    std::map<std::string, std::string> attributes{};

    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, language, path, command, preprocessedFile, //
                                   dependencyFile, treeFiles, attributes);

  private:
    DEF_SOL_UT_ACCESSOR(language);
    DEF_SOL_UT_ACCESSOR(path);
    DEF_SOL_UT_ACCESSOR(command);
    DEF_SOL_UT_ACCESSOR(preprocessedFile);
    DEF_SOL_UT_ACCESSOR(dependencyFile);
    DEF_SOL_UT_ACCESSOR(treeFiles);
    DEF_SOL_UT_ACCESSOR(attributes);

  public:
    DEF_TEAL_SOL_UT(Entry,                                  //
                    SOL_UT_FN_ACC(Entry, language),         //
                    SOL_UT_FN_ACC(Entry, path),             //
                    SOL_UT_FN_ACC(Entry, command),          //
                    SOL_UT_FN_ACC(Entry, preprocessedFile), //
                    SOL_UT_FN_ACC(Entry, dependencyFile),   //
                    SOL_UT_FN_ACC(Entry, treeFiles),        //
                    SOL_UT_FN_ACC(Entry, attributes));

    friend std::ostream &operator<<(std::ostream &os, const Entry &entry) {
      os << "sv::Database::Entry{"                                 //
         << ".language=" << entry.language << ", "                 //
         << ".path=" << entry.path << ", "                         //
         << ".command=" << entry.command << ", "                   //
         << ".preprocessedFile=" << entry.preprocessedFile << ", " //
         << ".dependencyFile=" << entry.dependencyFile << ", ";    //
      for (size_t i = 0; i < entry.treeFiles.size(); ++i)
        os << ".treeFiles[" << i << "]=(" << entry.treeFiles[i] << "), ";
      for (auto &[k, v] : entry.attributes)
        os << ".attributes[" << k << "]=" << v << ", ";
      os << "}";
      return os;
    }
  };

  std::string root{};
  std::vector<std::shared_ptr<Entry>> entries{};
  std::shared_ptr<PerFileCoverage> coverage{};

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
      os << ".entries[" << e->path << "]=" << e << ", ";
    os << "}";
    return os;
  }
};

} // namespace sv

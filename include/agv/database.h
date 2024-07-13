#pragma once

#include <ctime>
#include <iosfwd>
#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <utility>
#include <vector>

#include "glob.h"
#include "json.hpp"
#include "lua.h"
#include "tree.h"

namespace agv {

template <typename T> struct Memoized {
  mutable std::optional<T> value{};
  template <typename F> [[nodiscard]] const T &operator()(F f) const {
    if (!value) value = std::move(f());
    return *value;
  }
};

class Tree {
  Memoized<size_t> lazyNodes, lazyMaxDepth, lazyMaxWidth;

public:
  SemanticTree<std::string> root;
  explicit Tree(const SemanticTree<std::string> &root);
  [[nodiscard]] size_t nodes() const;
  [[nodiscard]] size_t maxDepth() const;
  [[nodiscard]] size_t maxWidth() const;
  [[nodiscard]] std::string prettyPrint() const;

  [[nodiscard]] static Tree combine(const std::string &rootName, const std::vector<Tree> &trees);
  [[nodiscard]] static Tree leaf(const std::string &rootName);
  [[nodiscard]] static Tree combine(const std::string &rootName,
                                    const sol::nested<std::vector<Tree>> &trees) {
    return Tree::combine(rootName, trees.value());
  }
  DEF_TEAL_SOL_UT(Tree,                         //
                  SOL_UT_FN(Tree, nodes),       //
                  SOL_UT_FN(Tree, maxDepth),    //
                  SOL_UT_FN(Tree, maxWidth),    //
                  SOL_UT_FN(Tree, prettyPrint), //
                  SOL_UT_FN(Tree, leaf),        //
                  SOL_UT_FN0(Tree, combine,
                             Tree (*)(const std::string &,
                                      const sol::nested<std::vector<Tree>> &)));
};

struct Range {
  uint32_t startByte, endByte;
  [[nodiscard]] std::string extract(const std::string &s) const {
    if (startByte > endByte || startByte >= s.size()) { return ""; }
    return s.substr(startByte, std::min(endByte, static_cast<uint32_t>(s.size())) - startByte);
  }

  bool operator<(const Range &that) const {
    return startByte == that.startByte ? endByte < that.endByte : startByte < that.startByte;
  }
  bool operator==(const Range &that) const {
    return startByte == that.endByte && endByte == that.endByte;
  }

private:
  DEF_SOL_UT_ACCESSOR(startByte)
  DEF_SOL_UT_ACCESSOR(endByte)
public:
  DEF_TEAL_SOL_UT(Range,                           //
                  SOL_UT_FN_ACC(Range, startByte), //
                  SOL_UT_FN_ACC(Range, endByte),   //
                  SOL_UT_FN(Range, extract));
  friend std::ostream &operator<<(std::ostream &os, const Range &range);
};

class Source {
  Memoized<size_t> lazySloc, lazyLloc;
  Memoized<std::set<uint32_t>> lazySlocLines;
  Memoized<std::set<Range>> lazyLlocRanges;
  Memoized<Tree> lazyTsTree;

public:
  TsTree tree;
  explicit Source(TsTree tree);
  [[nodiscard]] const std::string &content() const;
  [[nodiscard]] size_t sloc() const;
  [[nodiscard]] size_t lloc() const;
  [[nodiscard]] std::set<uint32_t> slocLines() const;
  [[nodiscard]] std::set<Range> llocRanges() const;
  [[nodiscard]] const Tree &tsTree() const;
  DEF_TEAL_SOL_UT(Source,                     //
                  SOL_UT_FN(Source, content), //
                  SOL_UT_FN(Source, sloc),    //
                  SOL_UT_FN(Source, lloc),    //
                  SOL_UT_FN(Source, tsTree));
};

class Unit {
  std::string path_, name_;
  Memoized<Source> lazySourceNormalised, lazySource;

public:
  Tree sTreeRoot, sTreeInlinedRoot, irTreeRoot;
  TsTree sourceRoot;

  Unit(std::string path, const SemanticTree<std::string> &sTree,
       const SemanticTree<std::string> &sTreeInlined, const SemanticTree<std::string> &irTree,
       TsTree source);
  [[nodiscard]] const std::string &path() const;
  [[nodiscard]] const std::string &name() const;
  [[nodiscard]] const Tree &sTree() const;
  [[nodiscard]] const Tree &sTreeInlined() const;
  [[nodiscard]] const Tree &irTree() const;
  [[nodiscard]] Source source(bool normalise) const;
  DEF_TEAL_SOL_UT(Unit,                          //
                  SOL_UT_FN(Unit, path),         //
                  SOL_UT_FN(Unit, name),         //
                  SOL_UT_FN(Unit, sTree),        //
                  SOL_UT_FN(Unit, sTreeInlined), //
                  SOL_UT_FN(Unit, irTree),       //
                  SOL_UT_FN(Unit, source));
  friend std::ostream &operator<<(std::ostream &os, const Unit &unit);
};

struct Codebase {
  std::string path;
  std::vector<std::shared_ptr<Unit>> units;

private:
  DEF_SOL_UT_ACCESSOR(path)
  DEF_SOL_UT_ACCESSOR(units)
public:
  DEF_TEAL_SOL_UT(Codebase, SOL_UT_FN_ACC(Codebase, path), SOL_UT_FN_ACC(Codebase, units));
  friend std::ostream &operator<<(std::ostream &os, const Codebase &codebase);
};

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
    friend std::ostream &operator<<(std::ostream &os, const Bitcode &bitcode);
  };

  struct Entry {
    std::vector<std::string> compileCommands{};
    std::string pchName{};
    std::vector<Bitcode> bitcodes{};
    std::map<std::string, std::string> dependencies{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, compileCommands, pchName, bitcodes, dependencies);

  private:
    DEF_SOL_UT_ACCESSOR(compileCommands)
    DEF_SOL_UT_ACCESSOR(pchName)
    DEF_SOL_UT_ACCESSOR(bitcodes)
    DEF_SOL_UT_ACCESSOR(dependencies)
  public:
    DEF_TEAL_SOL_UT(Entry,                                 //
                    SOL_UT_FN_ACC(Entry, compileCommands), //
                    SOL_UT_FN_ACC(Entry, pchName),         //
                    SOL_UT_FN_ACC(Entry, bitcodes),        //
                    SOL_UT_FN_ACC(Entry, dependencies));

    friend std::ostream &operator<<(std::ostream &os, const Entry &entry);
  };

  std::map<std::string, std::string> attributes{};
  std::string root{};
  std::map<std::string, Entry> entries{};
  std::map<std::string, Dependency> dependencies{};

  [[nodiscard]] static Database fromJsonString(const std::string &json);
  [[nodiscard]] static Database fromJsonStream(std::ifstream &stream);

  [[nodiscard]] static Database fromJsonFile(const std::string &file);
  [[nodiscard]] Codebase load(std::ostream &out,                     //
                              bool normalise,                        //
                              const std::string &path,               //
                              const std::vector<std::string> &roots, //
                              const std::function<bool(const std::string &)> &predicate) const;
  [[nodiscard]] Codebase load(bool normalise,                                     //
                              const std::string &path,                            //
                              const sol::nested<std::vector<std::string>> &roots, //
                              const std::function<bool(const std::string &)> &predicate) const {
    return load(std::cout, normalise, path, roots.value(), predicate);
  }
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
                  SOL_UT_FN(Database, fromJsonFile),     //
                  SOL_UT_FN0(Database, load,
                             Codebase (Database::*)(                            //
                                 bool,                                          //
                                 const std::string &,                           //
                                 const sol::nested<std::vector<std::string>> &, //
                                 const std::function<bool(const std::string &)> &) const

                             ));

  friend std::ostream &operator<<(std::ostream &os, const Database &database);
};

class Glob {
  std::regex regex;

public:
  explicit Glob(std::regex regex) : regex(std::move(regex)) {}
  [[nodiscard]] bool matches(const std::string &string) const {
    return std::regex_match(string, regex);
  }
  [[nodiscard]] static Glob pattern(const std::string &glob) {
    return Glob{agv::globToRegex(glob)};
  }
  DEF_TEAL_SOL_UT(Glob, SOL_UT_FN(Glob, pattern), SOL_UT_FN(Glob, matches)) //
};

} // namespace agv

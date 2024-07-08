#pragma once

#include <ctime>
#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <type_traits>
#include <vector>

#include "json.hpp"
#include "teal.h"
#include "tree.h"

namespace p3md {

namespace {

#define USERTYPE_FN0(Class, name, tpe)                                                             \
  std::tuple { #name, (tpe) & Class::name, (tpe) & Class::name }
#define USERTYPE_FN(Class, name)                                                                   \
  std::tuple { #name, &Class::name, &Class::name }
#define USERTYPE_RO(Class, name)                                                                   \
  std::tuple { #name, &Class::name, sol::readonly(&Class::name) }

template <typename C, typename... Args> auto bindUT(sol::state &s, const char *name, Args... args) {
  auto tpe = s.new_usertype<C>(name);
  ([&]() { tpe[std::get<0>(args)] = std::get<2>(args); }(), ...);
  return tpe;
}

#define USERTYPE_DEFINE(Class, ...)                                                                \
  static auto bind(sol::state &lua) { return bindUT<Class>(lua, #Class, __VA_ARGS__); }            \
  static void teal(teal::State &state) { teal::bind<Class>(state, #Class, __VA_ARGS__); }

} // namespace

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
  USERTYPE_DEFINE(Tree,                           //
                  USERTYPE_FN(Tree, nodes),       //
                  USERTYPE_FN(Tree, maxDepth),    //
                  USERTYPE_FN(Tree, maxWidth),    //
                  USERTYPE_FN(Tree, prettyPrint), //
                  USERTYPE_FN(Tree, leaf),        //
                  USERTYPE_FN0(Tree, combine,
                               Tree (*)(const std::string &,
                                        const sol::nested<std::vector<Tree>> &)));
};

class Source {
  Memoized<size_t> lazySloc, lazyLloc;
  Memoized<Tree> lazyTsTree;

public:
  TsTree tree;
  explicit Source(TsTree tree);
  [[nodiscard]] const std::string &content() const;
  [[nodiscard]] size_t sloc() const;
  [[nodiscard]] size_t lloc() const;
  [[nodiscard]] const Tree &tsTree() const;
  USERTYPE_DEFINE(Source,                       //
                  USERTYPE_FN(Source, content), //
                  USERTYPE_FN(Source, sloc),    //
                  USERTYPE_FN(Source, lloc),    //
                  USERTYPE_FN(Source, tsTree));
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
  USERTYPE_DEFINE(Unit,                            //
                  USERTYPE_FN(Unit, path),         //
                  USERTYPE_FN(Unit, name),         //
                  USERTYPE_FN(Unit, sTree),        //
                  USERTYPE_FN(Unit, sTreeInlined), //
                  USERTYPE_FN(Unit, irTree),       //
                  USERTYPE_FN(Unit, source));
};

struct Codebase {
  std::string path;
  std::vector<Unit> units;
  USERTYPE_DEFINE(Codebase,                    //
                  USERTYPE_RO(Codebase, path), //
                  USERTYPE_RO(Codebase, units));
};

struct Database {

  struct Dependency {
    std::time_t modified{};
    std::string content{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Dependency, modified, content);
    USERTYPE_DEFINE(Dependency,                        //
                    USERTYPE_RO(Dependency, modified), //
                    USERTYPE_RO(Dependency, content));
  };

  struct Bitcode {
    std::string name{};
    std::string kind{};
    std::string triple{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Bitcode, name, kind, triple);
    USERTYPE_DEFINE(Bitcode,                    //
                    USERTYPE_RO(Bitcode, name), //
                    USERTYPE_RO(Bitcode, kind), //
                    USERTYPE_RO(Bitcode, triple));
    friend std::ostream &operator<<(std::ostream &os, const Bitcode &bitcode);
  };

  struct Entry {
    std::vector<std::string> compileCommands{};
    std::string pchName{};
    std::vector<Bitcode> bitcodes{};
    std::map<std::string, std::string> dependencies{};
    NLOHMANN_DEFINE_TYPE_INTRUSIVE(Entry, compileCommands, pchName, bitcodes, dependencies);
    USERTYPE_DEFINE(Entry,                               //
                    USERTYPE_RO(Entry, compileCommands), //
                    USERTYPE_RO(Entry, pchName),         //
                    USERTYPE_RO(Entry, bitcodes),        //
                    USERTYPE_RO(Entry, dependencies));

    friend std::ostream &operator<<(std::ostream &os, const Entry &entry);
  };

  size_t clangMajorVersion{}, clangMinorVersion{}, clangPatchVersion{};
  std::map<std::string, Entry> entries{};
  std::map<std::string, Dependency> dependencies{};

  [[nodiscard]] static Database fromJson(const std::string &json);
  [[nodiscard]] static Database fromJsonFile(const std::string &file);
  [[nodiscard]] Codebase load(std::ostream &out,                     //
                              bool normalise,                        //
                              const std::string &path,               //
                              const std::vector<std::string> &roots, //
                              const std::function<bool(std::string)> &predicate) const;
  [[nodiscard]] Codebase load(bool normalise,                                     //
                              const std::string &path,                            //
                              const sol::nested<std::vector<std::string>> &roots, //
                              const std::function<bool(std::string)> &predicate) const {
    return load(std::cout, normalise, path, roots.value(), predicate);
  }
  NLOHMANN_DEFINE_TYPE_INTRUSIVE(Database,                                                //
                                 clangMajorVersion, clangMinorVersion, clangPatchVersion, //
                                 entries, dependencies);
  USERTYPE_DEFINE(Database,                                 //
                  USERTYPE_RO(Database, clangMajorVersion), //
                  USERTYPE_RO(Database, clangMinorVersion), //
                  USERTYPE_RO(Database, clangPatchVersion), //
                  USERTYPE_RO(Database, entries),           //
                  USERTYPE_RO(Database, dependencies),      //
                  USERTYPE_FN(Database, fromJson),          //
                  USERTYPE_FN(Database, fromJsonFile),      //
                  USERTYPE_FN0(Database, load,
                               Codebase (Database::*)(                            //
                                   bool,                                          //
                                   const std::string &,                           //
                                   const sol::nested<std::vector<std::string>> &, //
                                   const std::function<bool(std::string)> &) const

                               ));

  friend std::ostream &operator<<(std::ostream &os, const Database &database);
};

} // namespace p3md

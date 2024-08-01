#pragma once

#include <chrono>
#include <ctime>
#include <iosfwd>
#include <map>
#include <memory>
#include <mutex>
#include <ostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

#include "glob.h"
#include "lua.h"
#include "nlohmann/json.hpp"

#include "database.h"
#include "tree.h"

#include "semantic_ts.h"

namespace sv {

template <typename T> struct Memoized {
  mutable std::optional<T> value{};
  mutable std::once_flag flag{};
  mutable std::mutex mutex;
  Memoized() = default;
  ~Memoized() = default;
  Memoized(const Memoized &that) {
    std::scoped_lock lock(that.mutex);
    std::call_once(that.flag, [&] { value = that.value; });
  }
  Memoized &operator=(const Memoized &that) {
    if (this != &that) {
      std::scoped_lock lock(mutex, that.mutex);
      std::call_once(that.flag, [&] { value = that.value; });
    }
    return *this;
  }
  Memoized(Memoized &&that) noexcept {
    std::scoped_lock lock(that.mutex);
    std::call_once(that.flag, [&] { value = std::move(that.value); });
  }
  Memoized &operator=(Memoized &&that) noexcept {
    if (this != &that) {
      std::scoped_lock lock(mutex, that.mutex);
      std::call_once(that.flag, [&] { value = std::move(that.value); });
    }
    return *this;
  }
  template <typename F> [[nodiscard]] const T &operator()(F f) const {
    std::call_once(flag, [&]() { value = std::move(f()); });
    std::scoped_lock lock(mutex);
    return *value;
  }

  template <typename K> struct Parametric {
    mutable std::mutex mutex{};
    mutable std::unordered_map<K, Memoized<T>> xs{};
    template <typename F> [[nodiscard]] const T &operator()(K k, F f) const {
      std::scoped_lock lock(mutex);
      return xs[k](f);
    }
  };
};

template <typename T> class Cached {
  mutable std::optional<T> value{};
  std::chrono::milliseconds timeout;
  mutable std::chrono::steady_clock::time_point expiration{};
  mutable std::mutex mutex_;

public:
  explicit Cached(std::chrono::milliseconds timeout) : timeout(timeout) {}
  template <typename F> [[nodiscard]] const T &operator()(F f) const {
    auto now = std::chrono::steady_clock::now();
    {
      std::scoped_lock lock(mutex_);
      if (!value || now >= expiration) {
        value = std::move(f());
        expiration = now + timeout;
      }
    }
    return *value;
  }
};

class Tree {
  Memoized<size_t> lazyNodes, lazyMaxDepth, lazyMaxWidth;

public:
  NTree<SNode> root;
  explicit Tree(const NTree<SNode> &root);
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
  DEF_SOL_UT_ACCESSOR(startByte);
  DEF_SOL_UT_ACCESSOR(endByte);

public:
  DEF_TEAL_SOL_UT(Range,                           //
                  SOL_UT_FN_ACC(Range, startByte), //
                  SOL_UT_FN_ACC(Range, endByte),   //
                  SOL_UT_FN(Range, extract));
  friend std::ostream &operator<<(std::ostream &os, const Range &range);
};

class Source {
  using Mask = std::function<bool(const std::pair<uint32_t, uint32_t> &)>;

  Memoized<size_t> lazySloc, lazyLloc;
  Memoized<std::set<uint32_t>> lazySlocLines;
  Memoized<std::set<Range>> lazyLlocRanges;
  Memoized<std::string> lazyNormalisedContent;
  Memoized<Tree> lazyTsTree;
  TsTree root_;
  std::string content_;
  Mask mask;

public:
  Source(TsTree root, std::string content, const Mask &mask);
  [[nodiscard]] const std::string &content() const;
  [[nodiscard]] const std::string &contentWhitespaceNormalised() const;
  [[nodiscard]] size_t sloc() const;
  [[nodiscard]] size_t lloc() const;
  [[nodiscard]] std::set<uint32_t> slocLines() const;
  [[nodiscard]] std::set<Range> llocRanges() const;
  [[nodiscard]] const Tree &tsTree() const;
  DEF_TEAL_SOL_UT(Source,                                         //
                  SOL_UT_FN(Source, content),                     //
                  SOL_UT_FN(Source, contentWhitespaceNormalised), //
                  SOL_UT_FN(Source, sloc),                        //
                  SOL_UT_FN(Source, lloc),                        //
                  SOL_UT_FN(Source, tsTree));
};

struct FlatCoverage {

  struct EntryHash {
    std::size_t operator()(const std::pair<std::string, size_t> &p) const {
      auto hashFst = std::hash<std::string>{}(p.first);
      auto hashSnd = std::hash<size_t>{}(p.second);
      return hashFst ^ (hashSnd << 1);
    }
  };

  std::unordered_map<std::pair<std::string, size_t>, size_t, EntryHash> entries{};
  FlatCoverage() = default;
  explicit FlatCoverage(const PerFileCoverage &coverage);

private:
  DEF_SOL_UT_ACCESSOR(entries);

public:
  DEF_TEAL_SOL_UT(FlatCoverage, SOL_UT_FN_ACC(FlatCoverage, entries));
};

class Unit {

public:
  enum class View : uint8_t { AsIs, WithCoverage };

private:
  std::string path_, name_;
  Memoized<Source> lazySourceAsWritten{}, lazySourcePreprocessed{}, lazySourceWithCoverage{};
  Memoized<Tree>::Parametric<View> lazySTree{}, lazySTreeInlined{}, lazyIrTree{};
  Tree sTreeRoot, sTreeInlinedRoot, irTreeRoot;
  TsTree sourceRoot, preprocessedRoot;
  std::shared_ptr<FlatCoverage> coverage;

  [[nodiscard]] NTree<SNode> pruneTree(const NTree<SNode> &tree) const;
  [[nodiscard]] static TsTree normaliseTsTree(const TsTree &tree);

public:
  Unit(std::string path, const std::shared_ptr<FlatCoverage> &coverage,    //
       NTree<SNode> sTree, NTree<SNode> sTreeInlined, NTree<SNode> irTree, //
       TsTree source, TsTree preprocessedSource);
  [[nodiscard]] const std::string &path() const;
  [[nodiscard]] const std::string &name() const;

  [[nodiscard]] const Tree &sTree(View view) const;
  [[nodiscard]] const Tree &sTreeInlined(View view) const;
  [[nodiscard]] const Tree &irTree(View view) const;
  [[nodiscard]] const Source &sourceAsWritten() const;
  [[nodiscard]] const Source &sourcePreprocessed() const;
  [[nodiscard]] const Source &sourceWithCoverage() const;
  DEF_TEAL_SOL_UT(Unit,                             //
                  SOL_UT_FN(Unit, path),            //
                  SOL_UT_FN(Unit, name),            //
                  SOL_UT_FN(Unit, sTree),           //
                  SOL_UT_FN(Unit, sTreeInlined),    //
                  SOL_UT_FN(Unit, irTree),          //
                  SOL_UT_FN(Unit, sourceAsWritten), //
                  SOL_UT_FN(Unit, sourcePreprocessed));
  friend std::ostream &operator<<(std::ostream &os, const Unit &unit);
};

struct Codebase {
  std::string root;
  std::vector<std::shared_ptr<Unit>> units;
  std::shared_ptr<FlatCoverage> coverage;

private:
  DEF_SOL_UT_ACCESSOR(root);
  DEF_SOL_UT_ACCESSOR(units);
  DEF_SOL_UT_ACCESSOR(coverage);

public:
  [[nodiscard]] static Database loadDB(const std::string &dir);

  [[nodiscard]] static Codebase load(const Database &x,                     //
                                     std::ostream &out,                     //
                                     bool normalise,                        //
                                     const std::vector<std::string> &roots, //
                                     const std::function<bool(const std::string &)> &predicate);

  [[nodiscard]] static Codebase load(const Database &db,                                 //
                                     bool normalise,                                     //
                                     const sol::nested<std::vector<std::string>> &roots, //
                                     const std::function<bool(const std::string &)> &predicate) {
    return load(db, std::cout, normalise, roots.value(), predicate);
  }

  friend std::ostream &operator<<(std::ostream &os, const Codebase &codebase);

  DEF_TEAL_SOL_UT(Codebase,                          //
                  SOL_UT_FN_ACC(Codebase, root),     //
                  SOL_UT_FN_ACC(Codebase, units),    //
                  SOL_UT_FN_ACC(Codebase, coverage), //
                  SOL_UT_FN(Codebase, loadDB),
                  SOL_UT_FN0(Codebase, load,                                                   //
                             Codebase (*)(const Database &,                                    //
                                          bool,                                                //
                                          const sol::nested<std::vector<std::string>> &,       //
                                          const std::function<bool(const std::string &)> &))); //
};

class Glob {
  std::regex regex;

public:
  explicit Glob(std::regex regex) : regex(std::move(regex)) {}
  [[nodiscard]] bool matches(const std::string &string) const {
    return std::regex_match(string, regex);
  }
  [[nodiscard]] static Glob pattern(const std::string &glob) { return Glob{globToRegex(glob)}; }
  DEF_TEAL_SOL_UT(Glob, SOL_UT_FN(Glob, pattern), SOL_UT_FN(Glob, matches)) //
};

} // namespace sv

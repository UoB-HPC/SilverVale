#include <atomic>
#include <fstream>
#include <iostream>
#include <utility>

#include "sv/cli.h"
#include "sv/io.h"
#include "sv/model.h"
#include "sv/par.h"
#include "sv/tree_ts.h"

#include "tree_sitter_c/api.h"
#include "tree_sitter_cpp/api.h"
#include "tree_sitter_cuda/api.h"
#include "tree_sitter_fortran/api.h"
#include "tree_sitter_julia/api.h"
#include "tree_sitter_rust/api.h"

#include "aspartame/map.hpp"
#include "aspartame/set.hpp"
#include "aspartame/string.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace sv;

// === Tree ===
Tree::Tree(const NTree<SNode> &root) : root(root) {}
size_t Tree::nodes() const {
  return lazyNodes([&] {
    size_t n = 0;
    root.postOrderWalk([&](auto &, auto) { n++; });
    return n;
  });
}
size_t Tree::maxDepth() const {
  return lazyMaxDepth([&] {
    size_t maxDepth = 0;
    root.postOrderWalk([&](auto &, size_t depth) { maxDepth = std::max(maxDepth, depth); });
    return maxDepth;
  });
}
size_t Tree::maxWidth() const {
  return lazyMaxWidth([&] {
    std::vector<int> levelSize;
    root.postOrderWalk([&](auto &, size_t depth) {
      if (depth >= levelSize.size()) { levelSize.resize(depth + 1); }
      levelSize[depth]++;
    });
    return levelSize.empty() ? 0 : *std::max_element(levelSize.begin(), levelSize.end());
  });
}
Tree Tree::combine(const std::string &rootName, const std::vector<Tree> &trees, bool dropRoot) {
  return Tree{NTree<SNode>(SNode{rootName, Location{}},
                           dropRoot ? trees ^ bind([](auto &t) { return t.root.children; })
                                    : trees ^ map([](auto &t) { return t.root; }))};
}
std::string Tree::prettyPrint() const {
  std::stringstream stream;
  root.print(stream);
  return stream.str();
}
Tree Tree::leaf(const std::string &rootName) {
  return Tree{NTree<SNode>(SNode{rootName, Location{}}, {})};
}

// === Source ===

Source::Source(TsTree tree, std::string content, Mask mask)
    : root_(std::move(tree)), content_(std::move(content)), mask(std::move(mask)) {}
// XXX this content is different from that root_'s one as this can be modified for coverage
const std::string &Source::content() const { return content_; }

const std::string &Source::contentWhitespaceNormalised() const {
  return lazyNormalisedContent([&]() {
    // run a source-level (tree-unaware) WS normalisation here
    auto ws = [](char c) { return c == ' ' || c == '\t'; };
    std::string result;
    for (char c : content_) {
      if (result.empty()) {
        if (!ws(c) || c != '\n') result += c;
      } else {
        auto last = result.back();
        if (last == '\n' && c == '\n') continue; // delete consecutive NL
        if (ws(last) && ws(c)) continue;         // delete consecutive WS
        if (last == '\n' && ws(c)) continue;     // delete trailing WS after NL
        if (ws(last) && c == '\n') {
          result.pop_back();
          continue; // delete trailing WS before NL
        }
        result += c;
      }
    }
    return result;
  });
}
static std::pair<uint32_t, uint32_t> rowRange(TSNode n) {
  return {ts_node_start_point(n).row + 1,
          ts_node_end_point(n).row + 1}; // tree sitter row is 0-based
}

size_t Source::sloc() const {
  return lazySloc(
      [&] { return root_.slocLines([&](const TSNode n) { return mask(rowRange(n)); }).size(); });
}
size_t Source::lloc() const {
  return lazyLloc(
      [&] { return root_.llocRanges(([&](const TSNode n) { return mask(rowRange(n)); })).size(); });
}
std::set<uint32_t> Source::slocLines() const {
  return lazySlocLines([&] { return root_.slocLines(); });
}
std::set<Range> Source::llocRanges() const {
  return lazyLlocRanges([&] {
    return root_.llocRanges() ^ map([](auto &start, auto &end) { return Range{start, end}; });
  });
}
const Tree &Source::tsTree() const {
  return lazyTsTree([&] {
    auto root = root_.template traverse<NTree<TSNode>>(
        [&](const TSNode v) { return NTree{v, {}}; },
        [](auto &n, const auto &x) { n.children.emplace_back(x); });
    root.pruneInplace([&](const TSNode &n) { return mask(rowRange(n)); });
    return Tree(root.map<SNode>([&](const TSNode &n) {
      return SNode{std::string(ts_node_type(n)),
                   Location{.path = root_.name,
                            .line = ts_node_start_point(n).row + 1, // tree sitter row is 0-based
                            .col = ts_node_start_point(n).column}};
    }));
  });
}

// === AggregateSource

AggregateSource::AggregateSource(const std::vector<Source> &sources) : sources(sources) {}
std::vector<std::string> AggregateSource::content() const {
  return sources ^ map([](auto &s) { return s.content(); });
}
std::vector<std::string> AggregateSource::contentWhitespaceNormalised() const {
  return sources ^ map([](auto &s) { return s.contentWhitespaceNormalised(); });
}
size_t AggregateSource::sloc() const {
  return sources | fold_left(size_t{}, [](auto acc, auto &s) { return acc + s.sloc(); });
}
size_t AggregateSource::lloc() const {
  return sources | fold_left(size_t{}, [](auto acc, auto &s) { return acc + s.lloc(); });
}
const Tree &AggregateSource::tsTree() const {
  return lazyTsTree([&]() {
    return Tree::combine("root", sources ^ map([](auto &s) { return s.tsTree(); }), true);
  });
}

// === FlatCoverage ===

FlatCoverage::FlatCoverage(const PerFileCoverage &coverage) {
  for (auto &[file, instances] : coverage.instances) {
    for (auto &instance : instances) {
      if (instance.count != 0) {
        for (size_t line = instance.lineStart; line <= instance.lineEnd; ++line) {
          entries[std::pair{file, line}] += instance.count;
        }
      }
    }
  }
}

// === Unit ===

Unit::Unit(std::filesystem::path path,                    //
           const std::vector<std::regex> &rootGlobs,      //
           const std::shared_ptr<FlatCoverage> &coverage, //
           NTree<SNode> sTree,                            //
           NTree<SNode> sTreeInlined,                     //
           NTree<SNode> irTree,                           //
           std::vector<TsTree> sources,                   //
           std::vector<TsTree> preprocessedSources)
    : path_(std::move(path)), rootGlobs_(rootGlobs), sTreeRoot(std::move(sTree)),
      sTreeInlinedRoot(std::move(sTreeInlined)), irTreeRoot(std::move(irTree)),
      sourceRoots(std::move(sources)), //
      preprocessedRoots(std::move(preprocessedSources)), coverage(coverage) {}

std::string Unit::path() const { return path_; }
std::string Unit::name() const { return path_.filename(); }

static bool tailMatchNonEmpty(const std::string &l, const std::string &r) {
  if (l.empty() || r.empty()) return false;
  if (l.size() < r.size()) return r ^ ends_with(l);
  else return l ^ ends_with(r);
}

bool Unit::matchesRoots(const std::string &name) const {
  if (name.empty()) return false;
  auto self = path_.string();
  return tailMatchNonEmpty(self, name) ||
         rootGlobs_ ^ exists([&](const auto &r) { return std::regex_match(name, r); });
}

NTree<SNode> Unit::pruneTreeWithCoverage(const NTree<SNode> &tree) const {
  auto pruned = tree;
  pruned.pruneInplace([&](auto &s) {
    if (!matchesRoots(s.location.path)) return false;
    auto direct = coverage->entries.contains(std::pair{s.location.path, s.location.line});
    if (!direct) {
      return coverage->entries | exists([&](auto &covLoc, auto) {
               return s.location.line == covLoc.second &&
                      tailMatchNonEmpty(s.location.path, covLoc.first);
             });
    } else return direct;
  });
  return pruned;
}

NTree<SNode> Unit::pruneTreeSelf(const NTree<SNode> &tree) const {
  auto pruned = tree;
  pruned.pruneInplace([&](auto &s) { return matchesRoots(s.location.path); });
  return pruned;
}

const Tree &Unit::sTree(View view) const {
  return lazySTree(view, [view, this]() {
    switch (view) {
      case View::AsIs: return sTreeRoot;
      case View::Self: return Tree(pruneTreeSelf(sTreeRoot.root));
      case View::WithCov: return Tree(pruneTreeWithCoverage(sTreeRoot.root));
    }
  });
}
const Tree &Unit::sTreeInlined(View view) const {
  return lazySTreeInlined(view, [view, this]() {
    switch (view) {
      case View::AsIs: return sTreeInlinedRoot;
      case View::Self: return Tree(pruneTreeSelf(sTreeInlinedRoot.root));
      case View::WithCov: return Tree(pruneTreeWithCoverage(sTreeInlinedRoot.root));
    }
  });
}
const Tree &Unit::irTree(View view) const {
  return lazyIrTree(view, [view, this]() {
    switch (view) {
      case View::AsIs: return irTreeRoot;
      case View::Self: return Tree(pruneTreeSelf(irTreeRoot.root));
      case View::WithCov: return Tree(pruneTreeWithCoverage(irTreeRoot.root));
    }
  });
}

static TsTree normaliseTsTree(const TsTree &tree) {
  return tree.without("comment"); // // .normaliseWhitespaces();
}

const AggregateSource &Unit::sourceAsWritten() const {
  return lazySourceAsWritten([&]() {
    return AggregateSource( //
        sourceRoots ^ map([](auto &src) {
          auto normalised = normaliseTsTree(src);
          return Source(normalised, normalised.source, [](auto) { return true; });
        }));
  });
}
const AggregateSource &Unit::sourcePreprocessed() const {
  return lazySourcePreprocessed([&]() {
    return AggregateSource( //
        preprocessedRoots ^ map([](auto &src) {
          auto normalised = normaliseTsTree(src);
          return Source(normalised, normalised.source, [&](auto) { return true; });
        }));
  });
}
const AggregateSource &Unit::sourceWithCoverage() const {
  return lazySourceWithCoverage([&]() -> AggregateSource {
    // XXX use the original source but without comments to match up the lines
    return AggregateSource( //
        sourceRoots ^ map([&](auto &src) -> Source {
          auto normalised = normaliseTsTree(src);
          auto coveragePrunedSource =
              (normalised.source ^ lines()) | zip_with_index(size_t{1})        //
              | filter([&](auto, auto idx) {                                   //
                  return coverage->entries.contains(std::pair{src.name, idx}); //
                })                                                             //
              | keys()                                                         //
              | filter([](auto x) { return !(x ^ is_blank()); })               //
              | mk_string("\n");
          return Source(normalised, coveragePrunedSource, [&](auto &range) {
            return inclusive(range.first, range.second) | exists([&](auto line) {
                     return coverage->entries.contains(std::pair{path_, line});
                   });
          });
        }));
  });
}

// === Database ===

Database Codebase::loadDB(const std::string &root) {
  std::vector<std::shared_ptr<Database::Entry>> entries;
  auto coverage = std::make_shared<PerFileCoverage>();
  auto parseJSON = [](auto path) {
    std::ifstream s(path);
    s.exceptions(std::ios::failbit | std::ios::badbit);
    return nlohmann::json::parse(s);
  };
  try {
    for (auto &entry : std::filesystem::directory_iterator(root)) {
      if (auto path = entry.path(); path.string() ^ ends_with(EntrySuffix)) {
        if (path.filename() == EntryClangSBCCName) {
          ClangSBCCProfile p;
          nlohmann::from_json(parseJSON(path), p);
          for (auto &e : p.data) {
            for (auto &f : e.functions) {
              for (auto &filename : f.filenames ^ distinct()) {
                auto name = std::filesystem::path(filename);
                // XXX the first region encloses the entire function with a count of 1 iff it spans
                // more than one line
                if (f.regions.size() > 1 && f.regions[0].ExecutionCount == 1) {
                  // marker region: subtractive regions from this region onwards where 0 counts
                  // removes a region; we do this by first adding one instance per line then
                  // deleting it later; TODO this does not handle columns in any way
                  std::unordered_map<size_t, int64_t> counts;
                  for (size_t l = f.regions[0].LineStart; l <= f.regions[0].LineEnd; ++l)
                    counts.emplace(l, 1);
                  for (size_t i = 1; i < f.regions.size(); ++i) {
                    auto sub = f.regions[i];
                    int64_t delta =
                        sub.ExecutionCount > 0 ? int64_t(sub.ExecutionCount) : int64_t(-1);
                    for (size_t l = sub.LineStart; l <= sub.LineEnd; ++l)
                      counts[l] += delta;
                  }
                  for (auto &[line, count] : counts) {
                    coverage->instances[name].emplace_back(PerFileCoverage::Instance{
                        .function = f.name,
                        .lineStart = line,
                        .lineEnd = line,
                        .colStart = 0,
                        .colEnd = 0,
                        .count = static_cast<size_t>(count < 0 ? 0 : count)});
                  }
                } else {
                  for (auto &r : f.regions) {
                    coverage->instances[name].emplace_back(
                        PerFileCoverage::Instance{.function = f.name,
                                                  .lineStart = r.LineStart,
                                                  .lineEnd = r.LineEnd,
                                                  .colStart = r.ColumnStart,
                                                  .colEnd = r.ColumnEnd,
                                                  .count = r.ExecutionCount});
                  }
                }
              }
            }
          }
        } else if (path.filename() ^ ends_with(EntryGCCGCovName)) {
          // GCov is cumulative: it generates one profile per TU. We need to fold it into one.
          GCCGCovProfile p;
          nlohmann::from_json(parseJSON(path), p);
          for (auto &f : p.files) {
            for (auto &l : f.lines) {
              coverage->instances[std::filesystem::path(f.file)].emplace_back(
                  PerFileCoverage::Instance{.function = {},
                                            .lineStart = l.line_number,
                                            .lineEnd = l.line_number,
                                            .colStart = 0,
                                            .colEnd = 0,
                                            .count = l.count});
            }
          }
        } else try {
            auto p = std::make_shared<Database::Entry>();
            sv::readPacked<Database::Entry>(path, *p);
            entries.emplace_back(p);
          } catch (const std::exception &e) { SV_WARNF("Cannot load entry {}: {}", path, e); }
      }
    }
  } catch (const std::exception &e) { SV_WARNF("Cannot list directory {}: {}", root, e); }

  SV_INFOF("Loaded DB {} with {} entries and {} coverage entries", root, entries.size(),
           coverage->instances.size());

  return {root, entries, coverage};
}

Codebase Codebase::load(const Database &db,                        //
                        bool normalise,                            //
                        const std::vector<std::string> &rootGlobs, //
                        const std::function<bool(const std::string &)> &predicate) {

  auto rootGlobsRegexes = rootGlobs ^ map([](auto &r) { return globToRegex(r); });

  const auto createTsParser = [](const std::string &language) -> TSLanguage * {
    if (language == "c") return tree_sitter_c();
    else if (language == "cpp") return tree_sitter_cpp();
    else if (language == "fortran") return tree_sitter_fortran();
    else if (language == "cuda") return tree_sitter_cuda();
    else if (language == "julia") return tree_sitter_julia();
    else if (language == "rust") return tree_sitter_rust();
    else {
      SV_WARNF("Language {} is not supported by any included tree sitter parsers", language);
      return nullptr;
    }
  };

  const auto resolvePreprocessedSources = [&](const std::string &name, const std::string &iiLines,
                                              const std::string &language) -> std::vector<TsTree> {
    const auto [witnessed, contents] = parseCPPLineMarkers(iiLines);
    if (witnessed.empty()) {
      SV_WARNF("Nothing was witnessed while walking CPP markers in {}, tree will be empty", name);
      return {};
    }
    auto combined =
        (contents | collect([&](auto &name, auto &content) -> std::optional<TsTree> {
           if ((name == witnessed.front()) ||
               (rootGlobsRegexes ^ exists([&](auto &r) { return std::regex_match(name, r); }))) {
             return TsTree(name, content, createTsParser(language));
           }
           return std::nullopt;
         })               //
         | to_vector()) ^ // make sure the source is the last included and the order is stable
        sort_by([&](auto &t) { return std::pair{t.name == witnessed.front() ? 1 : 0, t.name}; });
    ;
    if (combined.empty()) { SV_WARNF("Empty preprocessed source for {}", name); }
    return combined;
  };

  auto resolveSources = [&](auto &deps, const std::string &needlePath,
                            const std::string &language) -> std::vector<TsTree> {
    auto combined =
        (deps | collect([&](auto &depPath, auto &dep) -> std::optional<TsTree> {
           if (depPath == needlePath ||
               (rootGlobsRegexes ^ exists([&](auto &r) { return std::regex_match(depPath, r); }))) {
             return TsTree(depPath, dep.content, createTsParser(language));
           }
           return std::nullopt;
         }) |
         to_vector()) ^ // make sure the source is the last included and the order is stable
        sort_by([&](auto &t) { return std::pair{t.name == needlePath ? 1 : 0, t.name}; });
    if (combined.empty()) { SV_WARNF("Empty source for {}", needlePath); }
    return combined;
  };

  auto flatCoverage =
      db.coverage ? std::make_shared<FlatCoverage>(*db.coverage) : std::make_shared<FlatCoverage>();

  const auto selected = db.entries                                            //
                        | filter([&](auto &x) { return predicate(x->path); }) //
                        | to_vector();                                        //
  auto units =
      par_map(selected, [&](const std::shared_ptr<Database::Entry> &x) -> std::shared_ptr<Unit> {
        try {
          auto loadTree = [&](std::string_view suffix) -> NTree<SNode> {
            auto file = x->treeFiles ^ filter([&](auto &f) { return f ^ ends_with(suffix); }) ^
                        head_maybe();
            if (!file) {
              SV_INFOF("Entry {} does not contain a {} suffixed tree file , ignoring...", x->path,
                       suffix);
              return {};
            }

            auto path = std::filesystem::path(db.root) / *file;
            if (!std::filesystem::exists(path)) {
              SV_WARNF("Entry recorded the tree file {} but it does not exists, using empty tree "
                       "instead",
                       path);
              return {};
            }
            return sv::readPacked<NTree<SNode>>(path);
          };
          auto dependencies = sv::readPacked<std::map<std::string, Dependency>>(x->dependencyFile);
          auto preprocessed = sv::readPacked<std::string>(x->preprocessedFile);
          auto unit = std::make_unique<Unit>(
              x->path, rootGlobsRegexes, flatCoverage,
              !normalise ? loadTree(EntryNamedSTreeSuffix) //
                         : loadTree(EntryUnnamedSTreeSuffix),
              !normalise ? loadTree(EntryNamedSTreeInlinedSuffix) //
                         : loadTree(EntryUnnamedSTreeInlinedSuffix),
              !normalise ? loadTree(EntryNamedIrTreeSuffix) //
                         : loadTree(EntryUnnamedIrTreeSuffix),
              resolveSources(dependencies, x->path, x->language),
              resolvePreprocessedSources(x->path, preprocessed, x->language));
          return unit;
        } catch (const std::exception &e) {
          SV_WARNF("Failed to load entry {}: {}", x->path, e);
          return {};
        }
      });
  return Codebase(db.root, units, flatCoverage);
}

namespace sv {
std::ostream &operator<<(std::ostream &os, const Range &range) {
  return os << "sv::Range{"                         //
            << ".start=" << range.startByte << ", " //
            << ".end=" << range.endByte             //
            << "}";
}

std::ostream &operator<<(std::ostream &os, const Codebase &codebase) {
  return os << "sv::Codebase{"                                        //
            << ".path=" << codebase.root                              //
            << ".units={" << (codebase.units ^ mk_string(",")) << "}" //
            << "}";
}
std::ostream &operator<<(std::ostream &os, const Unit &unit) {
  return os << "sv::Unit{"                                                                //
            << ".path=" << unit.path_ << ", "                                             //
            << ".sTreeRoot=(" << unit.sTreeRoot.nodes() << "), "                          //
            << ".sTreeInlinedRoot=(" << unit.sTreeInlinedRoot.nodes() << "), "            //
            << ".irTreeRoot=(" << unit.irTreeRoot.nodes() << "), "                        //
            << ".sourceRoots="                                                            //
            << (unit.sourceRoots ^ mk_string(", ", [](auto &t) { return t.name; }))       //
            << ".preprocessedRoots="                                                      //
            << (unit.preprocessedRoots ^ mk_string(", ", [](auto &t) { return t.name; })) //
            << "}";
}

} // namespace sv

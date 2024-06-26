#include <fstream>
#include <shared_mutex>
#include <utility>

#include "p3md/database.h"
#include "p3md/diff.h"
#include "p3md/glob.h"
#include "p3md/term.h"
#include "p3md/tree.h"

#include "llvm/Support/MemoryBuffer.h"

#include "aspartame/string.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "oneapi/tbb.h"

#include "dtl/dtl.hpp"
#include "tree_sitter_cpp/api.h"

namespace topdiff {
#include "apted_tree_index.h"
#include "node.h"
#include "unit_cost_model.h"
} // namespace topdiff

struct StringLabel {
  std::string label{};
  explicit StringLabel(std::string label) : label(std::move(label)) {}
  bool operator==(const StringLabel &other) const {
    return (label.compare(other.to_string()) == 0);
  }
  [[nodiscard]] unsigned int get_type() const { return 0; }
  [[nodiscard]] const std::string &get_label() const { return label; }
  [[nodiscard]] const std::string &to_string() const { return label; }
};

namespace std {
template <> struct hash<StringLabel> {
  typedef StringLabel argument_type;

  size_t operator()(StringLabel const &s) const noexcept {
    size_t const h(std::hash<std::string>{}(s.label));
    return h;
  }
};
} // namespace std

template <typename F> static void par_for(size_t n, F f) {
  tbb::parallel_for(size_t{}, n, [&](auto idx) { f(idx); });
}

template <typename C, typename F> static void par_for(C &&xs, F f) {
  tbb::parallel_for(size_t{}, xs.size(), [&](auto idx) { f(xs[idx], idx); });
}

using namespace aspartame;

static size_t longestCommonPrefixLen(const std::vector<std::string> &strings) {
  if (strings.size() < 2) return 0;
  auto sorted = strings ^ sort();
  auto &first = sorted.front();
  auto &last = sorted.back();
  size_t len = std::min(first.length(), last.length());
  size_t i = 0;
  while (i < len && first[i] == last[i])
    i++;
  return i;
}

template <typename T> static topdiff::node::Node<StringLabel> makeTree(const T &tree) {
  return tree.template traverse<topdiff::node::Node<StringLabel>>(
      [](const auto &v) { return topdiff::node::Node<StringLabel>(StringLabel(v)); },
      [](auto &n, const auto &x) { n.add_child(x); });
}

static double apTED(const topdiff::node::Node<StringLabel> &lhs,
                    const topdiff::node::Node<StringLabel> &rhs) {
  using CostModelLD = topdiff::cost_model::UnitCostModelLD<StringLabel>;
  using LabelDictionary = topdiff::label::LabelDictionary<StringLabel>;
  LabelDictionary ld;
  CostModelLD ucm(ld);

  topdiff::ted::APTEDTreeIndex<CostModelLD, topdiff::node::TreeIndexAPTED> apted_algorithm(ucm);
  topdiff::node::TreeIndexAPTED ti1;
  topdiff::node::index_tree(ti1, lhs, ld, ucm);
  topdiff::node::TreeIndexAPTED ti2;
  topdiff::node::index_tree(ti2, rhs, ld, ucm);
  return apted_algorithm.ted(ti1, ti2);
}

struct Model {

  struct Entry {
    std::string file;
    std::vector<p3md::TsTree> tsTree;
    p3md::SemanticTree<std::string> sTree;
    p3md::SemanticTree<std::string> sTreeInlined;

    [[nodiscard]] std::string fileName() const { return llvm::sys::path::filename(file).str(); }

    static Entry merge(const std::string &name, const std::vector<Entry> &xs) {

      //      p3md::TsTree()

      // xs[0].sTree

      Entry out{name, //
                {},   //
                p3md::SemanticTree<std::string>{"root", {}},
                p3md::SemanticTree<std::string>{"root", {}}};

      for (auto &x : xs) {
        out.tsTree.insert(out.tsTree.begin(), x.tsTree.begin(), x.tsTree.end());
        out.sTree.children.insert(out.sTree.children.end(), //
                                  x.sTree.children.begin(), //
                                  x.sTree.children.end());
        out.sTreeInlined.children.insert(out.sTreeInlined.children.end(), //
                                         x.sTreeInlined.children.begin(),
                                         x.sTreeInlined.children.end());
      }

      return out;
    }
  };

  std::string dir;
  std::unique_ptr<p3md::Database> db;
  std::unique_ptr<p3md::Database::Materialised> ast{};
  std::vector<Entry> entries{};

  static std::unique_ptr<Model> makeEmpty() { return std::make_unique<Model>("", nullptr); }

  static std::unique_ptr<Model> fromFile(const std::string &dbDir) {
    auto dbFile = dbDir + "/db.json";
    auto buffer = llvm::MemoryBuffer::getFile(dbFile, /*IsText*/ true);
    if (auto e = buffer.getError()) {
      std::cerr << "Cannot read db file " << dbFile << ": " << e.message() << std::endl;
      return nullptr;
    }
    auto model =
        std::make_unique<Model>(dbDir, p3md::Database::fromJson((*buffer)->getBuffer().str()));
    P3MD_COUT << "# Model inflated from " << dbFile << std::endl;
    return model;
  }

  void populate(const std::vector<std::string> &roots) {
    if (!ast) { ast = std::make_unique<p3md::Database::Materialised>(*db, dir); }
    entries.resize(ast->units.size());

    P3MD_COUT << "# Loading " << db->entries.size() << " entries and " << db->dependencies.size()
              << " dependencies." << std::endl;

    par_for(ast->units.size(), [&,                                        //
                                keys = ast->units | keys() | to_vector(), //
                                maxFileLen = static_cast<int>(
                                    (ast->units | keys()                                        //
                                     | map([](auto &k) { return k.size(); })                    //
                                     | reduce([](auto l, auto r) { return std::max(l, r); })) ^ //
                                    get_or_else(0))](auto idx) {
      auto &f = keys[idx];
      auto &unit = ast->units[f];
      auto &sm = unit->getSourceManager();
      auto data = sm.getBufferDataOrNone(sm.getMainFileID());
      if (data) {
        p3md::SemanticTree<std::string> sTree{"root", {}};
        p3md::SemanticTree<std::string> sTreeInlined{"root", {}};

        for (clang::Decl *decl :
             p3md::topLevelDeclsInMainFile(*unit) ^ sort_by([&](clang::Decl *decl) {
               return std::pair{sm.getDecomposedExpansionLoc(decl->getBeginLoc()).second,
                                sm.getDecomposedExpansionLoc(decl->getEndLoc()).second};
             })) {

          sm.getDecomposedLoc(decl->getBeginLoc());

          //              if(decl->isTemplated()  ) continue;

          auto createTree = [&](const p3md::TreeSemanticVisitor::Option &option) {
            p3md::SemanticTree<std::string> topLevel{"toplevel", {}};
            p3md::TreeSemanticVisitor(&topLevel, unit->getASTContext(), option).TraverseDecl(decl);
            return topLevel;
          };
          sTree.children.emplace_back(createTree({
              .inlineCalls = false,
              .normaliseVarName = false, //
              .normaliseFnName = false,  //
              .roots = roots             //
          }));
          sTreeInlined.children.emplace_back(createTree({
              .inlineCalls = true,
              .normaliseVarName = false, //
              .normaliseFnName = false,  //
              .roots = roots             //
          }));
        }

        //            static int a = 0;
        //            std::ofstream
        //            outFile(llvm::sys::path::filename(unit->getMainFileName()).str() + "."
        //            +
        //                                  std::to_string(a++) + ".txt");
        //            if (!outFile) { std::cerr << "Failed to open the file for writing!" <<
        //            std::endl; } sTree.print(std::identity(), outFile); outFile.close();

        auto tsTree = p3md::TsTree(data->str(), tree_sitter_cpp()).deleteNodes("comment");
        entries[idx] = {unit->getMainFileName().str(), {tsTree}, sTree, sTreeInlined};
      }
      P3MD_COUT << "# Loaded " << std::left << std::setw(maxFileLen) << f << "\r";
    });
    P3MD_COUT << std::endl;
  }
};

// template <typename Derived> struct DiffState {
//
//   virtual double max() const = 0;
//   virtual double diff(Derived &that) const = 0;

//
//  static std::variant<std::vector<p3md::TsTree>, topdiff::node::Node<StringLabel>>
//  fromEntry(const p3md::DataKind &kind, const Model::Entry &lhs) {
//    switch (kind) {
//      case p3md::DataKind::SLOC: [[fallthrough]];
//      case p3md::DataKind::LLOC: [[fallthrough]];
//      case p3md::DataKind::Source: return lhs.tsTree;
//      case p3md::DataKind::TSTree: {
//        topdiff::node::Node<StringLabel> root{StringLabel("root")};
//        for_each(lhs.tsTree, [&](auto &t) { root.add_child(makeTree(t)); });
//        return root;
//      }
//      case p3md::DataKind::STree: return makeTree(lhs.sTree);
//      case p3md::DataKind::STreeInline: return makeTree(lhs.sTreeInlined);
//      default:
//        std::cerr << "Unhandled kind" <<
//        static_cast<std::underlying_type_t<p3md::DataKind>>(kind)
//                  << std::endl;
//        std::abort();
//    }
//  }
//
//  p3md::DataKind kind;
//  std::variant<std::vector<p3md::TsTree>, topdiff::node::Node<StringLabel>> lhs;
//
//  DiffState(const p3md::DataKind &kind, const Model::Entry &lhs)
//      : kind(kind), lhs(fromEntry(kind, lhs)) {}
//
//  [[nodiscard]] double max() const {
//
//    switch (kind) {
//      case p3md::DataKind::SLOC:
//
//        auto m = lhs ^ get<std::vector<p3md::TsTree>>() ^ get_or_else(
//        std::vector<p3md::TsTree>{} );
//
//
//
//        break;
//      case p3md::DataKind::LLOC: break;
//      case p3md::DataKind::Source:
//
//
//
//        break;
//      case p3md::DataKind::TSTree: break;
//      case p3md::DataKind::STree: break;
//      case p3md::DataKind::STreeInline: break;
//    }
//
//    if (auto lhsSrc = lhs ^ get<std::vector<p3md::TsTree>>(); lhsSrc) {
//
//
//
//      return static_cast<double>(lhsSrc->size());
//    } else if (auto lhsTree = lhs ^ get<topdiff::node::Node<StringLabel>>(); lhsTree) {
//      return lhsTree->get_tree_size();
//    } else {
//      std::cerr << "Unexpected variant state " << std::endl;
//      std::abort();
//    }
//  }
//
//  [[nodiscard]] double diff(const Model::Entry &entry) const {
//    auto rhs = fromEntry(kind, entry);
//    if (auto lhsSrc = lhs ^ get<std::vector<p3md::TsTree>>(); lhsSrc) {
//      dtl::Diff<char, std::string> d(*lhsSrc, *(rhs ^ get<std::string>()));
//      d.compose();
//      return static_cast<double>(d.getEditDistance());
//    } else if (auto lhsTree = lhs ^ get<topdiff::node::Node<StringLabel>>(); lhsTree) {
//      return apTED(*lhsTree, *(rhs ^ get<topdiff::node::Node<StringLabel>>()));
//    } else {
//      std::cerr << "Unexpected variant state " << std::endl;
//      std::abort();
//    }
//  }
//};

class DiffState {
  p3md::DataKind kind;
  std::variant<std::vector<p3md::TsTree>, topdiff::node::Node<StringLabel>> data;

  template <typename T> [[nodiscard]] T as() const {
    return data ^ get<T>() ^ fold([&]() -> T {
             throw std::logic_error(
                 "Unexpected kind" +
                 std::to_string(static_cast<std::underlying_type_t<p3md::DataKind>>(kind)) +
                 " for tree diff");
           });
  }

public:
  DiffState(const p3md::DataKind &kind, const Model::Entry &lhs) : kind(kind) {
    switch (kind) {
      case p3md::DataKind::SLOC: [[fallthrough]];
      case p3md::DataKind::LLOC: [[fallthrough]];
      case p3md::DataKind::Source: data = lhs.tsTree; break;

      case p3md::DataKind::TSTree: {
        topdiff::node::Node<StringLabel> root{StringLabel("root")};
        for_each(lhs.tsTree, [&](auto &t) { root.add_child(makeTree(t)); });
        data = root;
        break;
      }
      case p3md::DataKind::STree: data = makeTree(lhs.sTree); break;
      case p3md::DataKind::STreeInline: data = makeTree(lhs.sTreeInlined); break;
    }
  }

  [[nodiscard]] double max() const {
    auto tsTreeMax = [&](auto f) {
      return as<std::vector<p3md::TsTree>>() | map(f) | fold_left(double{}, std::plus<>());
    };
    switch (kind) {
      case p3md::DataKind::SLOC: return tsTreeMax([&](auto &x) { return x.sloc(); });
      case p3md::DataKind::LLOC: return tsTreeMax([&](auto &x) { return x.lloc(); });
      case p3md::DataKind::Source: return tsTreeMax([&](auto &x) { return x.source.size(); });
      case p3md::DataKind::TSTree: [[fallthrough]];
      case p3md::DataKind::STree: [[fallthrough]];
      case p3md::DataKind::STreeInline:
        return as<topdiff::node::Node<StringLabel>>().get_tree_size();
    }
  }
  [[nodiscard]] double diff(const DiffState &that) const {
    switch (kind) {
      case p3md::DataKind::SLOC: [[fallthrough]];
      case p3md::DataKind::LLOC: return max() / that.max();
      case p3md::DataKind::Source:
        return as<std::vector<p3md::TsTree>>() | bind([&](auto x) {
                 return that.as<std::vector<p3md::TsTree>>() | map([&](auto y) {
                          dtl::Diff<char, std::string> d(x.source, y.source);
                          d.compose();
                          return static_cast<double>(d.getEditDistance());
                        });
               }) |
               fold_left(double{}, [](auto l, auto r) { return std::fmin(l, r); });
      case p3md::DataKind::TSTree: [[fallthrough]];
      case p3md::DataKind::STree: [[fallthrough]];
      case p3md::DataKind::STreeInline:
        return apTED(as<topdiff::node::Node<StringLabel>>(),
                     that.as<topdiff::node::Node<StringLabel>>());
    }
  }
};

struct DiffModel {

  using Entry = std::pair<std::string, std::vector<std::pair<std::string, double>>>;
  std::vector<std::string> models;
  std::map<p3md::DataKind, std::vector<Entry>> entries;
  std::shared_mutex mutex;

  explicit DiffModel(const std::vector<std::string> &models,
                     const std::vector<p3md::DataKind> &kinds, size_t entriesSize)
      : models(models), entries() {
    for (auto &k : kinds) {
      auto [it, _] = entries.emplace(k, std::vector<Entry>(entriesSize));
      for (auto &[header, xs] : it->second)
        xs.resize(models.size(), {"(invalid)", std::numeric_limits<double>::quiet_NaN()});
    }
  }

  void set(size_t rowIdx, p3md::DataKind kind, const std::string &file) {
    std::unique_lock<decltype(mutex)> lock;
    entries.find(kind)->second[rowIdx].first = file;
  }
  void set(size_t rowIdx, size_t colIdx, p3md::DataKind kind, const std::string &file,
           double value) {
    std::unique_lock<decltype(mutex)> lock;
    entries.find(kind)->second[rowIdx].second[colIdx] = {file, value};
  }

  void dump(const std::string &prefix) {
    auto dumpFile = [&](const std::string &name, auto consume) {
      std::ofstream out(name, std::ios::trunc);
      consume(out);
      P3MD_COUT << "# Wrote " << name << std::endl;
    };

    auto dumpEntries = [&](const std::string &name, auto select) {
      dumpFile(name, [&](std::ofstream &out) {
        models | prepend("entry") | prepend("kind") | mk_string(",") |
            for_each([&](auto &v) { out << v; });
        out << std::endl;
        for (auto &[kind, table] : entries) {
          for (auto &[head, row] : table) {
            out << (row | map([&](const auto &v) { return select(v); }) | prepend(head) |
                    prepend(std::string(to_string(kind))) | mk_string(","))
                << std::endl;
          }
        }
      });
    };
    dumpEntries(prefix + "model_map.csv", [](auto &p) { return p.first; });
    dumpEntries(prefix + "model_breakdown.csv", [](auto &p) { return std::to_string(p.second); });
    dumpFile(prefix + "model_total.csv", [&](std::ofstream &out) {
      out << (entries | keys() | map([](auto kind) { return std::string(to_string(kind)); }) |
              prepend("model") | mk_string(","))
          << std::endl;

      for (auto &[model, idx] : models | zip_with_index()) {
        out << (entries | values() | to_vector() | map([&](auto &table) {
                  return std::to_string(table |
                                        map([&](auto &, auto &row) { return row[idx].second; }) |
                                        fold_left(0.0, std::plus<>()));
                }) |
                prepend(model) | mk_string(","))
            << std::endl;
      }
    });
  }
};

class ProgressLogger {
  size_t total;
  int maxLogLength;
  std::atomic_size_t completed{};

public:
  ProgressLogger(size_t total, int maxLogLength) : total(total), maxLogLength(maxLogLength) {}
  void log(const std::string &line, bool progress = true) {
    auto s = P3MD_COUT << "# [" << (progress ? completed++ : completed.load()) << "/" << total
                       << "] " << std::left << std::setw(maxLogLength) << line;
    if (progress) (s << "\r").flush();
    else s << std::endl;
  }
};

int p3md::diff::run(const p3md::diff::Options &options) {

  tbb::global_control global_limit(tbb::global_control::max_allowed_parallelism,
                                   options.maxThreads);

  if (options.entries.empty()) {
    std::cerr << "At least 1 database required for comparison" << std::endl;
    return EXIT_FAILURE;
  }

  P3MD_COUT << "# Using base glob pattern: " << (options.baseGlobs ^ mk_string(", ")) << std::endl;
  P3MD_COUT << "# Using pair glob pattern: "
            << (options.entryGlobPairs.empty()
                    ? "(filename match)"
                    : (options.entryGlobPairs ^
                       mk_string(", ", [](auto &l, auto &r) { return l + " -> " + r; })))
            << std::endl;

  P3MD_COUT << "# Loading " << options.entries.size() << " models ..." << std::endl;
  std::vector<std::shared_ptr<Model>> models(options.entries.size());
  par_for(options.entries, [&](auto &entry, auto idx) {
    auto model = Model::fromFile(entry.first);
    model->populate(entry.second);
    models[idx] = std::move(model);
  });

  auto outputPrefix = (options.outputPrefix.empty() ? "" : options.outputPrefix + ".");
  auto lhsEntriesSorted =
      models[0]->entries                         //
      ^ map([](auto &e) { return std::ref(e); }) //
      ^ filter([&, regexes = options.baseGlobs ^
                             map([](auto &glob) { return globToRegex(glob); })](auto &e) {
          return regexes ^ exists([&](auto &r) { return std::regex_match(e.get().file, r); });
        }) //
      ^
      sort_by([](auto &x) {
        return -(x.get().tsTree |
                 map([&](auto &x) { return x.source ^ count([](auto c) { return c == '\n'; }); }) |
                 fold_left(0, std::plus<>()));
      });

  if (models.size() == 1) {
    auto commonPrefixLen = lhsEntriesSorted                            //
                           ^ map([](auto &e) { return e.get().file; }) //
                           ^ and_then(&longestCommonPrefixLen);        //

    auto model = DiffModel{
        lhsEntriesSorted //
            | map([&](auto &e) { return e.get().file.substr(commonPrefixLen); }) | to_vector(),
        options.kinds, lhsEntriesSorted.size()};

    auto logger =
        ProgressLogger{lhsEntriesSorted.size() * lhsEntriesSorted.size() * options.kinds.size(),
                       lhsEntriesSorted | fold_left(int{}, [](auto acc, auto &e) {
                         return std::max(acc, static_cast<int>(e.get().file.size()));
                       })};

    P3MD_COUT << "# Single model mode: comparing model against itself with "
              << (lhsEntriesSorted.size() * lhsEntriesSorted.size()) << " entries total."
              << std::endl;
    par_for(lhsEntriesSorted, [&](auto &lhsRef, auto lhsIdx) {
      par_for(options.kinds, [&, &lhs = lhsRef.get()](auto kind, auto) {
        model.set(lhsIdx, kind, lhs.file.substr(commonPrefixLen));
        par_for(lhsEntriesSorted, [&, state = DiffState{kind, lhs}](auto &rhsRef, auto rhsIdx) {
          auto &rhs = rhsRef.get();
          model.set(lhsIdx, rhsIdx, kind, rhs.fileName(), state.diff(DiffState{kind, rhs}));
          logger.log(rhs.file);
        });
        logger.log(lhs.file, false);
      });
    });
    P3MD_COUT << std::endl;
    model.dump(outputPrefix);
  } else {
    // XXX insert a null model after the reference model
    models.insert(std::next(models.begin()), Model::makeEmpty());

    auto commonPrefixLen = (models                                      //
                            | map([](auto &m) { return m->dir; })       //
                            | filter([](auto x) { return !x.empty(); }) //
                            | to_vector()) ^
                           and_then(&longestCommonPrefixLen);

    auto model = DiffModel{models ^ map([&](auto &m) {
                             return m->dir.empty() ? "(max)" : m->dir.substr(commonPrefixLen);
                           }),
                           options.kinds, lhsEntriesSorted.size()};

    auto globPairs = options.entryGlobPairs ^ map([](auto &baseGlob, auto &diffGlob) {
                       return std::pair{globToRegex(baseGlob), globToRegex(diffGlob)};
                     });
    auto logger = ProgressLogger{lhsEntriesSorted.size() * models.size() * options.kinds.size(),
                                 lhsEntriesSorted | fold_left(int{}, [](auto acc, auto &e) {
                                   return std::max(acc, static_cast<int>(e.get().file.size()));
                                 })};
    par_for(lhsEntriesSorted, [&](auto &lhsRef, auto lhsIdx) {
      auto &lhs = lhsRef.get();
      auto lhsFileName = lhs.fileName();
      auto diffGlob = globPairs ^ find([&](auto &baseGlob, auto &) {
                        return std::regex_match(lhsFileName, baseGlob);
                      });
      par_for(options.kinds, [&](auto kind, auto) {
        model.set(lhsIdx, kind, lhsFileName);
        par_for(models, [&, state = DiffState(kind, lhs)](auto &rhsRef, auto rhsIdx) {
          if (rhsRef->dir.empty()) {        // null model, set upper bounds
            model.set(lhsIdx, rhsIdx, kind, //
                      lhsFileName, state.max());
          } else {
            auto rhs = //
                rhsRef->entries ^ find([&](auto &rhs) {
                  return diffGlob ^
                         fold([&](auto &,
                                  auto &glob) { return std::regex_match(rhs.fileName(), glob); },
                              [&]() { return rhs.fileName() == lhsFileName; });
                });
            model.set(lhsIdx, rhsIdx, kind, //
                      rhs ? rhs->fileName() : "?",
                      rhs ? state.diff(DiffState{kind, *rhs})
                          : std::numeric_limits<double>::infinity());
          }
          logger.log(lhsFileName);
        });
      });
    });
    P3MD_COUT << std::endl;
    model.dump(outputPrefix);
  }

  return EXIT_SUCCESS;
}

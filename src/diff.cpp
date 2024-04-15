#ifdef __cpp_lib_syncbuf
  #include <syncstream>
  #define CSOUT std::osyncstream(std::cout)
#else
  #include <iostream>
  #define CSOUT std::cout
#endif

#include "p3md/database.h"
#include "p3md/diff.h"
#include "p3md/tree.h"

#include "llvm/Support/MemoryBuffer.h"

#include "aspartame/map.hpp"
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
  explicit StringLabel(const std::string &label) : label(label) {}
  bool operator==(const StringLabel &other) const {
    return (label.compare(other.to_string()) == 0);
  }
  unsigned int get_type() const { return 0; }
  const std::string &get_label() const { return label; }
  const std::string &to_string() const { return label; }
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
    p3md::TsTree tsTree;
    p3md::SemanticTree<std::string> sTree;
    p3md::SemanticTree<std::string> sTreeInlined;

    std::string fileName() const { return llvm::sys::path::filename(file).str(); }
  };

  std::string dir;
  std::unique_ptr<p3md::Database> db;
  std::unique_ptr<p3md::Database::Materialised> ast{};
  std::vector<Entry> entries{};

  static std::unique_ptr<Model> fromFile(const std::string &dbDir) {
    auto dbFile = dbDir + "/db.json";
    auto buffer = llvm::MemoryBuffer::getFile(dbFile, /*IsText*/ true);
    if (auto e = buffer.getError()) {
      std::cerr << "Cannot read db file " << dbFile << ": " << e.message() << std::endl;
      return nullptr;
    }
    auto model =
        std::make_unique<Model>(dbDir, p3md::Database::fromJson((*buffer)->getBuffer().str()));
    CSOUT << "# Model inflated from " << dbFile << std::endl;
    return model;
  }

  void populate(const std::vector<std::string> &roots) {
    if (!ast) { ast = std::make_unique<p3md::Database::Materialised>(*db); }
    entries.resize(ast->units.size());

    CSOUT << "# Loading " << db->entries.size() << " entries and " << db->dependencies.size()
          << " dependencies." << std::endl;

    auto maxFileLen = (ast->units | keys()                                        //
                       | map([](auto &k) { return k.size(); })                    //
                       | reduce([](auto l, auto r) { return std::max(l, r); })) ^ //
                      get_or_else(0);

    tbb::parallel_for(
        size_t{}, ast->units.size(), [&, keys = ast->units | keys() | to_vector()](auto idx) {
          auto &f = keys[idx];
          auto &unit = ast->units[f];
          auto &sm = unit->getSourceManager();
          auto data = sm.getBufferDataOrNone(sm.getMainFileID());
          if (data) {
            p3md::SemanticTree<std::string> sTree;
            p3md::SemanticTree<std::string> sTreeInlined;
            for (auto decl : p3md::topLevelDeclsInMainFile(*unit)) {
              auto createTree = [&](const p3md::TreeSemanticVisitor::Option &option) {
                p3md::SemanticTree<std::string> topLevel{"toplevel", {}};
                p3md::TreeSemanticVisitor(&topLevel, unit->getASTContext(), option)
                    .TraverseDecl(decl);
                return topLevel;
              };
              sTree.children.emplace_back(createTree({
                  .inlineCalls = false,
                  .normaliseVarName = true, //
                  .normaliseFnName = true,  //
                  .roots = roots            //
              }));
              sTreeInlined.children.emplace_back(createTree({
                  .inlineCalls = true,
                  .normaliseVarName = true, //
                  .normaliseFnName = true,  //
                  .roots = roots            //
              }));
            }
            auto tsTree = p3md::TsTree(data->str(), tree_sitter_cpp()).deleteNodes("comment");
            entries[idx] = {unit->getMainFileName().str(), tsTree, sTree, sTreeInlined};
          }
          CSOUT << "# Loaded " << std::left << std::setw(maxFileLen) << f << "\r";
        });
    CSOUT << std::endl;
  }
};

struct DiffState {

  static std::variant<std::string, topdiff::node::Node<StringLabel>>
  fromEntry(const p3md::DataKind &kind, const Model::Entry &lhs) {
    switch (kind) {
      case p3md::DataKind::Source: return lhs.tsTree.source;
      case p3md::DataKind::TSTree: return makeTree(lhs.tsTree);
      case p3md::DataKind::STree: return makeTree(lhs.sTree);
      case p3md::DataKind::STreeInline: return makeTree(lhs.sTreeInlined);
      default:
        std::cerr << "Unhandled kind" << static_cast<std::underlying_type_t<p3md::DataKind>>(kind)
                  << std::endl;
        std::abort();
    }
  }

  p3md::DataKind kind;
  std::variant<std::string, topdiff::node::Node<StringLabel>> lhs;

  DiffState(const p3md::DataKind &kind, const Model::Entry &lhs)
      : kind(kind), lhs(fromEntry(kind, lhs)) {}

  double diff(const Model::Entry &entry) const {
    auto rhs = fromEntry(kind, entry);
    if (auto lhsSrc = lhs ^ get<std::string>(); lhsSrc) {
      dtl::Diff<char, std::string> d(*lhsSrc, *(rhs ^ get<std::string>()));
      d.compose();
      return d.getEditDistance();
    } else if (auto lhsTree = lhs ^ get<topdiff::node::Node<StringLabel>>(); lhsTree) {
      return apTED(*lhsTree, *(rhs ^ get<topdiff::node::Node<StringLabel>>()));
    } else {
      std::cerr << "Unexpected variant state " << std::endl;
      std::abort();
    }
  }
};

int p3md::diff::run(const p3md::diff::Options &options) {

  if (options.entries.empty()) {
    std::cerr << "At least 1 database required for comparison" << std::endl;
    return EXIT_FAILURE;
  }

  CSOUT << "# Loading " << options.entries.size() << " models ..." << std::endl;
  std::vector<std::shared_ptr<Model>> models(options.entries.size());
  tbb::parallel_for(size_t{}, options.entries.size(), [&](auto idx) {
    auto model = Model::fromFile(options.entries[idx].first);
    model->populate(options.entries[idx].second);
    models[idx] = std::move(model);
  });

  auto &lhsModel = models[0];
  auto lhsEntriesSorted =
      lhsModel->entries ^ map([](auto &e) { return std::ref(e); }) ^ sort_by([](auto &x) {
        return -(x.get().tsTree.source ^ count([](auto c) { return c == '\n'; }));
      });

  std::vector<std::vector<std::string>> table(lhsModel->entries.size() + 1);

  if (models.size() == 1) {
    auto commonPrefixLen =
        lhsModel->entries ^ map([](auto &e) { return e.file; }) ^ and_then(&longestCommonPrefixLen);

    table[0] = lhsModel->entries | map([&](auto &e) { return e.file.substr(commonPrefixLen); }) |
               prepend("entry") | to_vector();
    auto completed = std::atomic_size_t{};
    auto total = lhsEntriesSorted.size() * lhsEntriesSorted.size();
    auto maxFilenameLength = lhsModel->entries | fold_left(size_t{}, [](auto acc, auto &e) {
                               return std::max(acc, e.file.size());
                             });
    CSOUT << "# Single model mode: comparing model against itself with " << total
          << " entries total." << std::endl;
    tbb::parallel_for(size_t{}, lhsEntriesSorted.size(), [&](auto outerIdx) {
      auto &lhs = lhsEntriesSorted[outerIdx].get();
      auto state = DiffState(options.kind, lhs);
      auto &row = table[outerIdx + 1];

      row.resize(lhsEntriesSorted.size() + 1);
      row[0] = lhs.file.substr(commonPrefixLen);
      tbb::parallel_for(size_t{}, lhsEntriesSorted.size(), [&](auto innerIdx) {
        auto &rhs = lhsEntriesSorted[innerIdx].get();
        row[innerIdx + 1] = std::to_string(state.diff(rhs));
        (CSOUT << "# [" << completed++ << "/" << total << "] " << std::left
               << std::setw(maxFilenameLength) << rhs.file << "\r")
            .flush();
      });
      (CSOUT << "# [" << completed << "/" << total << "] " << std::left
             << std::setw(maxFilenameLength) << lhs.file << std::endl)
          .flush();
    });
  } else {
    auto commonPrefixLen =
        models ^ map([](auto &m) { return m->dir; }) ^ and_then(&longestCommonPrefixLen);

    table[0] = models ^ map([&](auto &m) { return m->dir.substr(commonPrefixLen); });
    auto rhsModels = models ^ tail();

    auto completed = std::atomic_size_t{};
    auto total = lhsEntriesSorted.size() * rhsModels.size();
    auto maxFilenameLength = lhsModel->entries | fold_left(size_t{}, [](auto acc, auto &e) {
                               return std::max(acc, e.file.size());
                             });

    tbb::parallel_for(size_t{}, lhsEntriesSorted.size(), [&](auto lhsEntriesIdx) {
      auto &lhs = lhsEntriesSorted[lhsEntriesIdx].get();
      auto state = DiffState(options.kind, lhs);
      auto lhsFileName = lhs.fileName();
      auto &row = table[lhsEntriesIdx + 1];
      row.resize(rhsModels.size() + 1);
      row[0] = lhsFileName;
      tbb::parallel_for(size_t{}, rhsModels.size(), [&](auto rhsModelIdx) {
        auto rhs = rhsModels[rhsModelIdx]->entries ^
                   find([&](auto &rhs) { return rhs.fileName() == lhsFileName; });
        if (rhs) {
          row[rhsModelIdx + 1] = std::to_string(state.diff(*rhs));
        } else {
          row[rhsModelIdx + 1] = std::to_string(std::numeric_limits<double>::infinity());
        }
        (CSOUT << "# [" << completed++ << "/" << total << "] " << std::left
               << std::setw(maxFilenameLength) << lhsFileName << "\r")
            .flush();
      });
    });
  }

  CSOUT << std::endl;
  for (auto row : table) {
    CSOUT << (row | mk_string(",")) << std::endl;
  }
  return EXIT_SUCCESS;
}

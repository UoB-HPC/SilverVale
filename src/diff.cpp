#include <utility>
#include <fstream>

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

    [[nodiscard]] std::string fileName() const { return llvm::sys::path::filename(file).str(); }
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
    P3MD_COUT << "# Model inflated from " << dbFile << std::endl;
    return model;
  }

  void populate(const std::vector<std::string> &roots) {
    if (!ast) { ast = std::make_unique<p3md::Database::Materialised>(*db); }
    entries.resize(ast->units.size());

    P3MD_COUT << "# Loading " << db->entries.size() << " entries and " << db->dependencies.size()
              << " dependencies." << std::endl;

    tbb::parallel_for(
        size_t{}, ast->units.size(),
        [&,                                        //
         keys = ast->units | keys() | to_vector(), //
         maxFileLen =
             static_cast<int>((ast->units | keys()                                        //
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
                  .normaliseVarName = false, //
                  .normaliseFnName = false,  //
                  .roots = roots            //
              }));
            }


            static int a =0;
            std::ofstream outFile(llvm::sys::path::filename(unit->getMainFileName() ).str()+ "."  + std::to_string(a++) +".txt");
            if (!outFile) {
              std::cerr << "Failed to open the file for writing!" << std::endl;
            }
            sTreeInlined.print(std::identity(), outFile);
            outFile.close();

            auto tsTree = p3md::TsTree(data->str(), tree_sitter_cpp()).deleteNodes("comment");
            entries[idx] = {unit->getMainFileName().str(), tsTree, sTree, sTreeInlined};
          }
          P3MD_COUT << "# Loaded " << std::left << std::setw(maxFileLen) << f << "\r";
        });
    P3MD_COUT << std::endl;
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

  [[nodiscard]] double diff(const Model::Entry &entry) const {
    auto rhs = fromEntry(kind, entry);
    if (auto lhsSrc = lhs ^ get<std::string>(); lhsSrc) {
      dtl::Diff<char, std::string> d(*lhsSrc, *(rhs ^ get<std::string>()));
      d.compose();
      return static_cast<double>(d.getEditDistance());
    } else if (auto lhsTree = lhs ^ get<topdiff::node::Node<StringLabel>>(); lhsTree) {
      return apTED(*lhsTree, *(rhs ^ get<topdiff::node::Node<StringLabel>>()));
    } else {
      std::cerr << "Unexpected variant state " << std::endl;
      std::abort();
    }
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
  tbb::parallel_for(size_t{}, options.entries.size(), [&](auto idx) {
    auto model = Model::fromFile(options.entries[idx].first);
    model->populate(options.entries[idx].second);
    models[idx] = std::move(model);
  });

  auto regexes = options.baseGlobs ^ map([](auto &glob) { return globToRegex(glob); });
  auto &lhsModel = models[0];
  auto lhsEntriesSorted =
      lhsModel->entries                          //
      ^ map([](auto &e) { return std::ref(e); }) //
      ^ filter([&](auto &e) {
          return regexes ^ exists([&](auto &r) { return std::regex_match(e.get().file, r); });
        }) //
      ^ sort_by([](auto &x) {
          return -(x.get().tsTree.source ^ count([](auto c) { return c == '\n'; }));
        });

  std::vector<std::vector<std::string>> diffTable(lhsEntriesSorted.size() + 1);
  std::vector<std::vector<std::string>> fileTable(diffTable.size());

  if (models.size() == 1) {
    auto commonPrefixLen = lhsEntriesSorted                            //
                           ^ map([](auto &e) { return e.get().file; }) //
                           ^ and_then(&longestCommonPrefixLen);        //

    diffTable[0] = lhsEntriesSorted                                                     //
                   | map([&](auto &e) { return e.get().file.substr(commonPrefixLen); }) //
                   | prepend("entry")                                                   //
                   | to_vector();
    fileTable[0] = diffTable[0];

    auto completed = std::atomic_size_t{};
    auto total = lhsEntriesSorted.size() * lhsEntriesSorted.size();
    auto maxFilenameLength = lhsEntriesSorted | fold_left(int{}, [](auto acc, auto &e) {
                               return std::max(acc, static_cast<int>(e.get().file.size()));
                             });
    P3MD_COUT << "# Single model mode: comparing model against itself with " << total
              << " entries total." << std::endl;
    tbb::parallel_for(size_t{}, lhsEntriesSorted.size(), [&](auto outerIdx) {
      auto &lhs = lhsEntriesSorted[outerIdx].get();
      auto state = DiffState(options.kind, lhs);
      auto &diffRow = diffTable[outerIdx + 1];
      auto &fileRow = fileTable[outerIdx + 1];
      diffRow.resize(lhsEntriesSorted.size() + 1);
      fileRow.resize(diffRow.size());
      diffRow[0] = lhs.file.substr(commonPrefixLen);
      fileRow[0] = diffRow[0];

      tbb::parallel_for(size_t{}, lhsEntriesSorted.size(), [&](auto innerIdx) {
        auto &rhs = lhsEntriesSorted[innerIdx].get();
        diffRow[innerIdx + 1] = std::to_string(state.diff(rhs));
        fileRow[innerIdx + 1] = rhs.fileName();
        (P3MD_COUT << "# [" << completed++ << "/" << total << "] " << std::left
                   << std::setw(maxFilenameLength) << rhs.file << "\r")
            .flush();
      });
      (P3MD_COUT << "# [" << completed << "/" << total << "] " << std::left
                 << std::setw(maxFilenameLength) << lhs.file << std::endl)
          .flush();
    });
  } else {
    auto commonPrefixLen =
        models ^ map([](auto &m) { return m->dir; }) ^ and_then(&longestCommonPrefixLen);

    diffTable[0] = models ^ map([&](auto &m) { return m->dir.substr(commonPrefixLen); });
    fileTable[0] = diffTable[0];

    auto rhsModels = models ^ tail();
    auto completed = std::atomic_size_t{};
    auto total = lhsEntriesSorted.size() * rhsModels.size();
    auto maxFilenameLength = lhsEntriesSorted | fold_left(int{}, [](auto acc, auto &e) {
                               return std::max(acc, static_cast<int>(e.get().file.size()));
                             });

    auto globPairs = options.entryGlobPairs ^ map([](auto &baseGlob, auto &diffGlob) {
                       return std::pair{globToRegex(baseGlob), globToRegex(diffGlob)};
                     });

    tbb::parallel_for(size_t{}, lhsEntriesSorted.size(), [&](auto lhsEntriesIdx) {
      auto &lhs = lhsEntriesSorted[lhsEntriesIdx].get();
      auto state = DiffState(options.kind, lhs);
      auto lhsFileName = lhs.fileName();
      auto &diffRow = diffTable[lhsEntriesIdx + 1];
      auto &fileRow = fileTable[lhsEntriesIdx + 1];
      diffRow.resize(rhsModels.size() + 1);
      fileRow.resize(diffRow.size());
      diffRow[0] = lhsFileName;
      fileRow[0] = diffRow[0];

      auto diffGlob = globPairs ^ find([&](auto &baseGlob, auto &) {
                        return std::regex_match(lhsFileName, baseGlob);
                      });

      tbb::parallel_for(size_t{}, rhsModels.size(), [&](auto rhsModelIdx) {
        auto rhs = //
            rhsModels[rhsModelIdx]->entries ^ find([&](auto &rhs) {
              return diffGlob ^
                     fold(
                         [&](auto &, auto &glob) { return std::regex_match(rhs.fileName(), glob); },
                         [&]() { return rhs.fileName() == lhsFileName; });
            });

        if (rhs) {
          diffRow[rhsModelIdx + 1] = std::to_string(state.diff(*rhs));
          fileRow[rhsModelIdx + 1] = rhs->fileName();
        } else {
          diffRow[rhsModelIdx + 1] = std::to_string(std::numeric_limits<double>::infinity());
          fileRow[rhsModelIdx + 1] = "?";
        }
        (P3MD_COUT << "# [" << completed++ << "/" << total << "] " << std::left
                   << std::setw(maxFilenameLength) << lhsFileName << "\r")
            .flush();
      });
    });
  }
  P3MD_COUT << std::endl;
  for (auto row : fileTable) {
    P3MD_COUT << (row | mk_string(",")) << std::endl;
  }
  P3MD_COUT << std::endl;
  for (auto row : diffTable) {
    P3MD_COUT << (row | mk_string(",")) << std::endl;
  }
  return EXIT_SUCCESS;
}

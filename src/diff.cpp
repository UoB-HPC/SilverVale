#include "p3md/diff.h"
#include "p3md/database.h"
#include "p3md/tree.h"

#include <iostream>
#include <thread>

#include "llvm/Support/MemoryBuffer.h"

#include "aspartame/unordered_map.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "dtl/dtl.hpp"
#include "tree_sitter_cpp/api.h"

#include "apted_tree_index.h"
#include "node.h"
#include "oneapi/tbb.h"
#include "string_label.h"
#include "unit_cost_model.h"

using namespace aspartame;

void makeTDNode() {
  auto stree = root.traverse<node::Node<label::StringLabel>>(
      [](auto &v) { return node::Node<label::StringLabel>(v); },
      [](auto &n, const auto &x) { n.add_child(x); });

  auto source = data->str();
  auto tsTree = TsTree(source, tree_sitter_cpp()).deleteNodes("comment");
  auto tstree = tsTree.traverse<node::Node<label::StringLabel>>(
      [](auto v) { return node::Node<label::StringLabel>(std::string(v)); },
      [](auto &n, const auto &x) { n.add_child(x); });
}

struct Model {

  struct Entry {
    std::string filename;
    p3md::TsTree tsTree;
    p3md::SemanticTree<std::string> sTree;
  };

  std::unique_ptr<p3md::Database> db;
  std::unique_ptr<p3md::Database::Materialised> ast{};
  std::vector<Entry> entries{};

  static std::unique_ptr<Model> fromFile(const std::string &dbFile) {
    auto buffer = llvm::MemoryBuffer::getFile(dbFile, /*IsText*/ true);
    if (auto e = buffer.getError()) {
      std::cerr << "Cannot read db file " << dbFile << ": " << e.message() << std::endl;
      return nullptr;
    }
    return std::make_unique<Model>(p3md::Database::fromJson((*buffer)->getBuffer().str()));
  }

  void populate(const std::vector<std::string> &roots) {
    if (!ast) { ast = std::make_unique<p3md::Database::Materialised>(*db); }
    for (auto &[f, unit] : ast->units) {
      auto &sm = unit->getSourceManager();
      auto data = sm.getBufferDataOrNone(sm.getMainFileID());
      if (data) {
        p3md::SemanticTree<std::string> sTree;
        for (auto decl : p3md::topLevelDeclsInMainFile(*unit)) {
          p3md::SemanticTree<std::string> topLevel{"toplevel", {}};
          p3md::TreeSemanticVisitor V(&topLevel, unit->getASTContext(),
                                      p3md::TreeSemanticVisitor::Option{
                                          .normaliseVarName = true, //
                                          .normaliseFnName = true,  //
                                          .roots = roots            //
                                      });
          V.TraverseDecl(decl);
          sTree.children.emplace_back(topLevel);
        }
        auto tsTree = p3md::TsTree(data->str(), tree_sitter_cpp()).deleteNodes("comment");
        entries.emplace_back(unit->getMainFileName().str(), tsTree, sTree);
      }
    }
  }
};

int p3md::diff::run(const p3md::diff::Options &options) {

  if (options.entries.size() <= 1) {
    std::cerr << "At least 2 databases required for comparison, got " << options.entries.size()
              << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Loading " << options.entries.size() << " database(s) ..." << std::endl;

  std::vector<std::shared_ptr<p3md::Database>> databases(options.entries.size());
  std::vector<std::shared_ptr<p3md::Database::Materialised>> materialised(options.entries.size());
  tbb::parallel_for(static_cast<std::size_t>(0), options.entries.size(), [&](auto idx) {
    auto &[file, roots] = options.entries[idx];
    databases[idx] = std::move(readDb(file + "/db.json"));
    if (!databases[idx]) {
      std::cerr << "Cannot load left database" << std::endl;
    } else {
      materialised[idx] = std::make_unique<p3md::Database::Materialised>(*databases[idx]);
    }
  });

  if (!materialised[0]) {
    std::cerr << "Cannot load base database " << options.entries[0].first << std::endl;
    return EXIT_FAILURE;
  }

  if (auto failed = materialised | drop(1) | zip_with_index() | collect([&](auto &x, auto idx) {
                      return x ? std::nullopt : std::optional{options.entries[idx].first};
                    }) |
                    to_vector();
      !failed.empty()) {
    std::cerr << "Warning: The following " << failed.size()
              << " database(s) failed to load and will be ignored:\n";
    for (auto &f : failed)
      std::cerr << " " << f << "\n";
  }

  materialised | zip_with_index() | for_each([&](auto &x, auto idx) {
    auto &[file, roots] = options.entries[idx];
    std::cout << std::setw(6) << (idx == 0 ? "Base " : "    ") << " " << x->units.size()
              << " entries (" << file << ", roots=" << (roots ^ mk_string(", ")) << ")\n";
  });

  //  std::vector<std::vector<Chunk>> outs(2);
  //

  for (auto idx : {0, 1}) {
    for (auto &[f, ast] : materialised[idx]->units) {
      std::cout << f << std::endl;
      //      p3md::SemanticTree<std::string> root;
      //      for (auto decl : p3md::topLevelDeclsInMainFile(*ast)) {
      //        p3md::SemanticTree<std::string> topLevel{"toplevel", {}};
      //        p3md::TreeSemanticVisitor V(&topLevel, ast->getASTContext(),
      //                                    p3md::TreeSemanticVisitor::Option{
      //                                        .normaliseVarName = false, //
      //                                        .normaliseFnName = false,  //
      //                                        .roots = options.leftRoots //
      //                                    });
      //        V.TraverseDecl(decl);
      //        root.children.emplace_back(topLevel);
      //        //      root.print(std::identity(), std::cout);
      //      }
      //      auto t = root.traverse<node::Node<label::StringLabel>>(
      //          0, [](auto &v) { return node::Node<label::StringLabel>(v); },
      //          [](auto &n, const auto &x) { n.add_child(x); });
      //      outs[idx].emplace_back(ast->getMainFileName().str(), t);

      auto &sm = ast->getSourceManager();

      auto data = sm.getBufferDataOrNone(sm.getMainFileID());
      if (data) {
        auto str = data->str();

        auto tsast = TsTree(str, tree_sitter_cpp()).deleteNodes("comment");

        //                p3md::printTree<TSTree>(
        //                    0, {}, tsast.root(), std::cout,
        //                    [](const TSTree &n) { return std::string(ts_node_type(n)); },
        //                    [&](const TSTree &n) -> std::vector<TSTree> {
        //                      return iota<uint32_t>(0, ts_node_child_count(n))              //
        //                             | map([&](uint32_t i) { return ts_node_child(n, i); }) //
        //                             | to_vector();
        //                    });

        //        outs[idx].emplace_back(ast->getMainFileName().str(), str, makeTSTree(0,
        //        tsast.root()));
      }
    }
  }
  //
  //  std::cout << outs[0].size() << std::endl;
  //  std::cout << outs[1].size() << std::endl;
  //
  //  for (const auto &lc : outs[0] ^ sort_by([](auto &c) { return c.filename; })) {
  //    for (const auto &rc : outs[1]) {
  //
  //      if (llvm::sys::path::filename(lc.filename) != llvm::sys::path::filename(rc.filename)) {
  //        continue;
  //      }
  //
  //      using Label = label::StringLabel;
  //      using CostModelLD = cost_model::UnitCostModelLD<Label>;
  //      using LabelDictionary = label::LabelDictionary<Label>;
  //
  //      LabelDictionary ld;
  //      CostModelLD ucm(ld);
  //      ted::APTEDTreeIndex<CostModelLD, node::TreeIndexAPTED> apted_algorithm(ucm);
  //      node::TreeIndexAPTED ti1;
  //      node::TreeIndexAPTED ti2;
  //      node::index_tree(ti1, lc.node, ld, ucm);
  //      node::index_tree(ti2, rc.node, ld, ucm);
  //      double ted = apted_algorithm.ted(ti1, ti2);
  //
  //      dtl::Diff<char, std::string> d(lc.source, rc.source);
  //      d.compose();
  //      auto sourceED = d.getEditDistance();
  //
  //      std::cout << llvm::sys::path::filename(lc.filename).str() << "," << ted << "," << sourceED
  //                << std::endl;
  //    }
  //  }

  return EXIT_SUCCESS;
}

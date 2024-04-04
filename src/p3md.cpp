#include <iostream>
#include <memory>

#include "tree_sitter/api.h"
#include "tree_sitter_c/api.h"
#include "tree_sitter_cpp/api.h"
#include "tree_sitter_fortran/api.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "p3md/options.h"
#include "p3md/p3md.h"
#include "p3md/tree.h"

// #include "apted_tree_index.h"
// #include "node.h"
// #include "string_label.h"
// #include "unit_cost_model.h"

using namespace aspartame;
using namespace clang;
using namespace clang::tooling;

// node::Node<label::StringLabel> makeTree(int depth, const TSNode &node) {
//   node::Node<label::StringLabel> n{{std::string(ts_node_type(node))}};
//   for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
//     n.add_child(makeTree(depth + 1, ts_node_child(node, i)));
//   }
//   return n;
// }

std::vector<Decl *> p3md::topLevelDeclsInMainFile(ASTUnit &unit) {
  std::vector<Decl *> xs;
  unit.visitLocalTopLevelDecls(&xs, [](auto xsPtr, auto decl) {
    reinterpret_cast<decltype(xs) *>(xsPtr)->push_back(const_cast<Decl *>(decl));
    return true;
  });
  return xs ^
         filter([&](Decl *d) { return unit.getSourceManager().isInMainFile(d->getLocation()); });
}

int p3md::diff_main(p3md::DataKind kind, int argc, const char **argv) {
  auto maybeOptions = p3md::DiffOptions::parse(argc, argv);
  if (auto x = maybeOptions.takeError()) {
    llvm::errs() << maybeOptions.takeError();
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

int p3md::dump_main(p3md::DataKind kind, int argc, const char **argv) {
  auto maybeOptions = p3md::DumpOptions::parse(argc, argv);
  if (auto x = maybeOptions.takeError()) {
    llvm::errs() << maybeOptions.takeError();
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

int p3md::list_main(int argc, const char **argv) {
  auto maybeOptions = p3md::ListOptions::parse(argc, argv);
  if (auto x = maybeOptions.takeError()) {
    llvm::errs() << maybeOptions.takeError();
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

int p3md::build_main(int argc, const char **argv) {
  auto maybeOptions = p3md::BuildOptions::parse(argc, argv);
  if (auto x = maybeOptions.takeError()) {
    llvm::errs() << maybeOptions.takeError();
    return EXIT_FAILURE;
  }

    auto &options = maybeOptions.get();


  llvm::outs() //
      << "Build:\n"
      << " - Root:     " << (options.BuildDir) << "\n"
      << " - DB:       "
      << (options.CompilationDBFile.empty() ? "(ad-hoc)" : options.CompilationDBFile) << "\n"
      << " - Commands: " << options.Compilations->getAllCompileCommands().size() << "\n";
  llvm::outs() << "Roots:\n";
  for (auto &root : options.Roots)
    llvm::outs() << " - " << root << "\n";
  llvm::outs() << "Sources:\n";


  options.Compilations->getAllFiles() ^ for_each([&](auto x){
    std::cout << ""<<(options.resolve(x) ^ get_or_else("???")) << std::endl;

                                        });

  auto all = options.SourcePathList ^ exists([](auto x){return x == "*"; });

  for (auto &source : options.SourcePathList)
    llvm::outs() << " - " << source << " => "
                 << (options.resolve(source) ^ get_or_else("(not found, skipped)")) << "\n";
  llvm::outs() << "Adjustments: " << (options.Adjuster({"${ARGS}"}, "${FILE}") | mk_string(" "))
               << "\n";

  auto that = std::make_unique<ArgumentsAdjustingCompilations>(std::move(options.Compilations));
  that->appendArgumentsAdjuster(getInsertArgumentAdjuster( CommandLineArguments{
                                                              "-Wno-unknown-attributes",
                                                              "-Wno-deprecated-declarations",
                                                               "-fno-sycl",
//                                                              "-v",
                                                              "-I/usr/lib/clang/17/include",
                                                              "-I/opt/intel/oneapi/compiler/2024.1/include",
                                                              "-I/opt/intel/oneapi/compiler/2024.1/include/sycl",

                                                          } , ArgumentInsertPosition::END));

    ClangTool Tool(*that,
                   options.SourcePathList ^ collect([&](auto x) { return options.resolve(x); }));

//  for (auto &f : options.SourcePathList ^ collect([&](auto x) { return options.resolve(x); })) {
//
//    auto buffer = llvm::MemoryBuffer::getFile(f);
//
//    if (std::error_code EC = buffer.getError()) {
//      std::cerr << "Failed to open " + f + ": " + EC.message();
//      return EXIT_FAILURE;
//    }
//
//    for (auto cc : options.Compilations->getCompileCommands(f)) {
//
//      std::cout << "@@@ "<< (cc.CommandLine ^ mk_string(" ")) << "\n";
//
//      auto unit =
//          buildASTFromCodeWithArgs((*buffer)->getBuffer(),
//                                   cc.CommandLine ^ drop(1) ^ append("-v") ^
//                                       append("-I/opt/intel/oneapi/compiler/2024.1/include/sycl") ^
//                                       append("-I/opt/intel/oneapi/compiler/2024.1/include") ^
//                                       append("-I/usr/lib/clang/17/include") ^
//                                       filter([](auto a) { return !(a ^ starts_with("-fsycl")); }) ^
//                                       filter([](auto a) { return !(a ^ starts_with("-Xsycl")); }),
//                                   llvm::sys::path::filename(f));
//
//      auto decls = topLevelDeclsInMainFile(*unit);
//
//      std::cout << decls.size() << "\n";
//      decls[0]->dump();
//
//
//    }
//
//    //  std::cout << (options.Compilations->getCompileCommands(f) ^ mk_string("\n",
//    //  [](CompileCommand cc){
//    //                  return cc.CommandLine ^ mk_string(" ");
//    //                })) << "\n";
//  }

//  return EXIT_SUCCESS;



  std::vector<std::unique_ptr<ASTUnit>> xs;
    if (auto result = Tool.buildASTs(xs); result != 0) {
      llvm::errs() << "Build AST failed: " << result << "\n";
      return result;
    }
    std::cout << xs.size() << "\n";
  //
    IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts2 = new DiagnosticOptions();
    auto diagnostics = CompilerInstance::createDiagnostics(DiagOpts2.get());

  //  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
  //  IntrusiveRefCntPtr<DiagnosticIDs> diagnosticIDs(new DiagnosticIDs());
  //  IntrusiveRefCntPtr<DiagnosticsEngine> diagnostics(
  //      new DiagnosticsEngine(diagnosticIDs, DiagOpts.get(),
  //                            new TextDiagnosticPrinter(llvm::errs(), DiagOpts.get()), true));

  llvm::vfs::InMemoryFileSystem mem;

  xs[0]->Save("foo.ast");
  //  auto opt = std::make_shared<clang::HeaderSearchOptions>();
  //  auto that = ASTUnit::LoadFromASTFile("foo.ast",                           //
  //                                       clang::RawPCHContainerReader(),      //
  //                                       ASTUnit::WhatToLoad::LoadEverything, //
  //                                       diagnostics,                         //
  //                                       clang::FileSystemOptions(""),        //
  //                                       opt);
  //  xs[0] = std::move(that);

  std::cout << xs.size() << "\n";

  //  using Label = label::StringLabel;
  //  using CostModelLD = cost_model::UnitCostModelLD<Label>;
  //  using LabelDictionary = label::LabelDictionary<Label>;

  //  std::vector<node::Node<Label>> trees;
  for (auto &unit : xs) {

    std::cout << unit->getMainFileName().str() << "\n";

    auto keep = topLevelDeclsInMainFile(*unit);

    keep | for_each([&](Decl *x) {
      if (auto fn = llvm::dyn_cast<FunctionDecl>(x); fn) {
        // No prototype, skip
        if (!fn->getPreviousDecl()) { return; }
      }

      p3md::SemanticNode<std::string> root{"root", {}};
      p3md::TreeSemanticVisitor V(&root, unit->getASTContext(),
                                  p3md::TreeSemanticVisitor::Option{
                                      .normaliseVarName = false, //
                                      .normaliseFnName = false,  //
                                      .roots = options.Roots     //
                                  });
      V.TraverseDecl(x);

//      root.print(std::identity(), std::cout);

//      nlohmann::json a = root;
      //      a.dump(2);

      //      clang::ASTWriter w();

      //      std::cout << std::setw(2) << a << "\n";

//            x->dump();
    });

    TSParser *parser = ts_parser_new();
    ts_parser_set_language(parser, tree_sitter_cpp());
    auto x = unit->getSourceManager().getBufferData(unit->getSourceManager().getMainFileID());
    const char *source_code = x.data();
    //    std::cout << source_code << "\n";

    TSTree *tree = ts_parser_parse_string(parser, nullptr, source_code, std::strlen(source_code));

    TSNode root_node = ts_tree_root_node(tree);
    //    walk<TSNode>(
    //        0, {}, root_node, [](const TSNode &n) { return std::string(ts_node_type(n)); },
    //        [&](const TSNode &n) -> std::vector<TSNode> {
    //          return iota<uint32_t>(0, ts_node_child_count(n))              //
    //                 | map([&](uint32_t i) { return ts_node_child(n, i); }) //
    //                 | to_vector();
    //        });

    //    trees.emplace_back(makeTree(0, root_node));

    //    std::cout << ts_node_child_count(root_node) << "\n";

    //    char *string = ts_node_string(root_node);
    //    printf("Syntax tree: %s\n", string);
    //
    //    std::free(string);
    ts_tree_delete(tree);
    ts_parser_delete(parser);
  }

  //  LabelDictionary ld;
  //  CostModelLD ucm(ld);
  //  ted::APTEDTreeIndex<CostModelLD, node::TreeIndexAPTED> apted_algorithm(ucm);
  //  node::TreeIndexAPTED ti1;
  //  node::TreeIndexAPTED ti2;
  //
  //  node::index_tree(ti1, trees[0], ld, ucm);
  //  node::index_tree(ti2, trees[1], ld, ucm);
  //  std::cout << "Distance TED:" << apted_algorithm.ted(ti1, ti2) << std::endl;
  //    return Tool.run(newFrontendActionFactory<clang::SyntaxOnlyAction>().get());
  return EXIT_SUCCESS;
}

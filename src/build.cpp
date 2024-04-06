#include <fstream>
#include <iostream>
#include <memory>
#include <thread>

#include "tree_sitter/api.h"
#include "tree_sitter_c/api.h"
#include "tree_sitter_cpp/api.h"
#include "tree_sitter_fortran/api.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticBuffer.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

#include "aspartame/optional.hpp"
#include "aspartame/set.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "p3md/build.h"
#include "p3md/glob.h"
#include "p3md/p3md.h"
#include "p3md/tree.h"

#include <zlib.h>

#include <sys/stat.h>
#include <sys/types.h>

using namespace aspartame;
using namespace clang;
using namespace clang::tooling;

// #include "apted_tree_index.h"
// #include "node.h"
// #include "string_label.h"
// #include "unit_cost_model.h"

class gz_ostream : public llvm::raw_ostream {
  gzFile GzFile;

public:
  gz_ostream(const std::string &name, std::error_code &code) : GzFile(gzopen(name.c_str(), "wb")) {
    if (!GzFile) {
      code = std::make_error_code(static_cast<std::errc>(errno));
      errno = 0;
    }
  }
  ~gz_ostream() override { gzclose(GzFile); }
  void write_impl(const char *Ptr, size_t Size) override { gzwrite(GzFile, Ptr, Size); }
  [[nodiscard]] uint64_t current_pos() const override { return gztell(GzFile); }
  void flush_nonempty() {
    // Flushing gzip stream. It doesn't flush the underlying OS.
    gzflush(GzFile, Z_FINISH);
  }
};

static std::optional<std::pair<std::unique_ptr<CompilationDatabase>, std::string>>
findCompilationDatabaseFromDirectory(StringRef root) {
  while (!root.empty()) {
    std::string discard;
    if (std::unique_ptr<CompilationDatabase> DB =
            CompilationDatabase::loadFromDirectory(root, discard)) {
      return std::pair{std::move(DB), (root + "/compile_command.json").str()};
    }
    root = llvm::sys::path::parent_path(root);
  }
  return {};
}

std::optional<std::string> p3md::build::Options::resolve(const std::string &sourcePath) const {
  if (llvm::sys::path::is_absolute(sourcePath)) {
    return llvm::sys::fs::is_regular_file(sourcePath) ? std::optional{sourcePath} : std::nullopt;
  }
  for (auto &root : rootDirs) {
    if (llvm::sys::fs::is_regular_file(root + "/" += sourcePath)) return sourcePath;
  }
  return {};
}

ArgumentsAdjuster p3md::build::Options::resolveAdjuster() const {
  return combineAdjusters(getInsertArgumentAdjuster(argsBefore, ArgumentInsertPosition::BEGIN),
                          getInsertArgumentAdjuster(argsAfter, ArgumentInsertPosition::END));
}

std::unique_ptr<CompilationDatabase>
p3md::build::Options::resolveDatabase(ArgumentsAdjuster adjuster) const {
  auto root = buildDir;
  while (!root.empty()) {
    std::string ignored;
    if (auto db = CompilationDatabase::loadFromDirectory(root, ignored); db) {
      auto AdjustingCompilations = std::make_unique<ArgumentsAdjustingCompilations>(std::move(db));
      AdjustingCompilations->appendArgumentsAdjuster(adjuster);
      return std::move(AdjustingCompilations);
    }
    root = llvm::sys::path::parent_path(root);
  }
  return {};
}

// node::Node<label::StringLabel> makeTree(int depth, const TSNode &node) {
//   node::Node<label::StringLabel> n{{std::string(ts_node_type(node))}};
//   for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
//     n.add_child(makeTree(depth + 1, ts_node_child(node, i)));
//   }
//   return n;
// }

std::vector<Decl *> p3md::build::topLevelDeclsInMainFile(ASTUnit &unit) {
  std::vector<Decl *> xs;
  unit.visitLocalTopLevelDecls(&xs, [](auto xsPtr, auto decl) {
    reinterpret_cast<decltype(xs) *>(xsPtr)->push_back(const_cast<Decl *>(decl));
    return true;
  });
  return xs ^
         filter([&](Decl *d) { return unit.getSourceManager().isInMainFile(d->getLocation()); });
}

static int clearAndCreateDir(bool clear, const std::string &outDir) {
  auto dirOp = [&outDir](const std::string &op, auto f) {
    if (auto err = f(); err != std::errc()) {
      std::cerr << "Failed to " << op << " " << outDir << ": " << err.message() << "\n";
      return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
  };

  if (clear) {
    auto result = EXIT_SUCCESS;
    result = dirOp("remove", [&]() { return llvm::sys::fs::remove_directories(outDir); });
    if (result != EXIT_SUCCESS) return result;
    result = dirOp("create", [&]() { return llvm::sys::fs::create_directories(outDir); });
    if (result != EXIT_SUCCESS) return result;
    return result;
  } else {
    auto result = EXIT_SUCCESS;
    result = dirOp("create", [&]() { return llvm::sys::fs::create_directories(outDir); });
    if (result != EXIT_SUCCESS) return result;
    std::error_code err;
    auto begin = llvm::sys::fs::directory_iterator(outDir, err);
    if (err != std::errc()) {
      std::cerr << "Failed to traverse output directory " << outDir << ": " << err.message()
                << "\n";
      return EXIT_FAILURE;
    }
    if (begin != llvm::sys::fs::directory_iterator()) {
      std::cerr << "Output directory " << outDir
                << " not empty, use --clear to clear output directory\n";
      return EXIT_FAILURE;
    }
  }
}
struct Task {
  size_t idx;
  std::string sourceName;
  std::string pchName;
  std::error_code error;
  std::shared_ptr<raw_ostream> stream;
  struct Result {
    std::string sourceName;
    std::optional<std::string> pchName;
    std::string diagnostic;
    std::unordered_set<std::string> dependentSources;
  };
};

static std::vector<Task::Result> buildPCHParallel(const CompilationDatabase &db,
                                                  const std::vector<std::string> &files,
                                                  std::string outDir, size_t unitsPerThread) {

  auto [success, failed] =
      (files | zip_with_index() | map([&](auto file, auto idx) {
         llvm::SmallString<128> nameGZ;
         llvm::sys::path::append(nameGZ, outDir,
                                 std::to_string(idx) + "." + llvm::sys::path::filename(file).str() +
                                     ".pch.gz");
         Task task{idx, file, nameGZ.c_str(), std::make_error_code(std::errc()), nullptr};
         auto stream = std::make_shared<llvm::raw_fd_stream>(task.pchName, task.error);
         //         auto stream = std::make_shared<gz_ostream>(task.pchName, task.error);
         if (task.error == std::errc()) task.stream = std::move(stream);
         return task;
       }) |
       to_vector()) ^
      partition([](auto &t) { return t.error == std::errc(); });

  auto maxFileLength = files ^ fold_left(0, [](auto acc, auto &s) {
                         return std::max(acc, static_cast<int>(s.size()));
                       });
  std::atomic_size_t completed{};
  std::vector<Task::Result> results(success.size());
  auto threads =
      (success | grouped(unitsPerThread)             //
       | map([](auto v) { return v | to_vector(); }) //
       | to_vector()) ^
      map([&](auto &tasks) {
        return std::thread([&, tasks, total = files.size()]() {
          for (auto task : tasks) {
            std::string messageStorage;
            llvm::raw_string_ostream message(messageStorage);

            IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
            TextDiagnosticPrinter diagPrinter(message, DiagOpts.get());

            ClangTool Tool(db, {task.sourceName});
            Tool.setDiagnosticConsumer(&diagPrinter);
            std::vector<std::unique_ptr<ASTUnit>> out;
            auto result = Tool.buildASTs(out);
            diagPrinter.finish();
            if (result != 0)
              message << "# Clang-Tool exited with non-zero code: " << result << "\n";
            std::cout << "[" << completed++ << "/" << total << "] " << task.sourceName
                      << std::setw(maxFileLength) << "\r";
            std::cout.flush();
            if (out.size() != 1)
              message << "# More than one AST unit produced; "
                      << "input command is ill-formed and only the first unit will be preserved\n";

            if (out[0]->serialize(*task.stream)) // XXX true is fail
              message << "# Serialisation failed\n";

            results[task.idx] = {task.sourceName, task.pchName, messageStorage};
            auto &sm = out[0]->getSourceManager();
            view(sm.fileinfo_begin(), sm.fileinfo_end())                                       //
                | map([](auto entry) { return entry.getFirst()->tryGetRealPathName().str(); }) //
                | filter([](auto s) { return !s.empty(); })                                    //
                | for_each([&](auto x) { results[task.idx].dependentSources.emplace(x); });    //
          }
        });
      });
  for (auto &t : threads)
    t.join();

  std::cout << std::endl;

  success.clear(); // drop the streams so the file can close
  for (auto t : failed)
    results.emplace_back(t.sourceName, std::nullopt, t.error.message());
  return results;
}

int p3md::build::run(const p3md::build::Options &options) {

  std::cout //
      << "Build:\n"
      << " - Build:        " << options.buildDir << " (compile_commands.json, ...)\n"
      << " - Output:       " << options.outDir << "\n"
      << " - Clear output: " << (options.clearOutDir ? "yes" : "no") << "\n"
      << " - Max threads:  " << options.maxThreads << "\n";
  std::cout << "Roots:\n";
  for (auto &root : options.rootDirs)
    std::cout << " - " << root << "\n";

  auto adjuster = options.resolveAdjuster();

  adjuster = combineAdjusters(
      adjuster, getInsertArgumentAdjuster(
                    CommandLineArguments{
                        "-Wno-unknown-attributes",
                        "-Wno-deprecated-declarations",
                        "-fno-sycl",
                        //                                                              "-v",
                        "-I/usr/lib/clang/17/include",
                        "-I/opt/intel/oneapi/compiler/2024.1/include",
                        "-I/opt/intel/oneapi/compiler/2024.1/include/sycl",
                    },
                    ArgumentInsertPosition::END));

  std::shared_ptr<CompilationDatabase> db = options.resolveDatabase(adjuster);
  if (!db) {
    std::cerr << "Unable to open compilation database at build dir" << options.buildDir
              << ", please check if compile_commands.json exists in that directory\n";
    return EXIT_FAILURE;
  }
  auto regex = globToRegex(options.sourceGlob);
  auto allFiles = db->getAllFiles() ^ collect([&](auto x) { return options.resolve(x); });
  auto files = allFiles ^ filter([&](auto file) { return std::regex_match(file, regex); }) ^ sort();
  std::cout << "Adjustments: " << (adjuster({"${ARGS}"}, "${FILE}") | mk_string(" ")) << "\n";
  std::cout << "Sources (" << files.size() << "/" << allFiles.size() << "):\n";
  for (auto &file : files) {
    std::cout << " - " << file << "\n";
  }

  if (auto result = clearAndCreateDir(options.clearOutDir, options.outDir);
      result != EXIT_SUCCESS) {
    return result;
  }

  auto unitsPerThread =
      static_cast<size_t>(std::ceil(static_cast<double>(files.size()) / options.maxThreads));

  std::cout << "Scheduling " << unitsPerThread << " unit(s) per thread for a total of "
            << options.maxThreads << " thread(s)" << std::endl;

  auto xs = buildPCHParallel(*db, files, options.outDir, unitsPerThread);

  xs ^ for_each([&](auto &r) {
    std::cout << r.sourceName << " " << (r.pchName ? *r.pchName : "(failed)") << " = \n"
              << " = " << r.diagnostic.size() << "\n";

    IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts2 = new DiagnosticOptions();
    auto diagnostics = CompilerInstance::createDiagnostics(DiagOpts2.get());

    if (r.pchName) {

      IntrusiveRefCntPtr<llvm::vfs::InMemoryFileSystem> VFS = new llvm::vfs::InMemoryFileSystem();

      std::ifstream stream(r.pchName->c_str(), std::ios::in | std::ios::binary);
      std::vector<char> contents((std::istreambuf_iterator<char>(stream)),
                                 std::istreambuf_iterator<char>());

      //
      //      char outbuffer[1024*64];
      //      auto infile =  fopen(r.pchName->c_str(), "rb");
      ////      frewind(infile);
      //
      //      std::vector<char> outfile;
      //      while(!feof(infile))
      //      {
      //        int len = fread( (void*)outbuffer, sizeof(outbuffer),1, infile);
      //        outfile.insert(outfile.end(), outbuffer, outbuffer+len);
      //      }
      //      fclose(infile);

      auto mb = llvm::MemoryBuffer::getMemBuffer(llvm::StringRef(contents.data(), contents.size()),
                                                 "", false);
      VFS->addFile(*r.pchName + "a", 1, std::move(mb));

      for (auto f : r.dependentSources) {

        struct stat rr;
        if (stat(f.c_str(), &rr) == 0) {
          auto mod_time = rr.st_mtime;
          if (f.empty()) continue;
          std::cout << f << "\n";
          auto mb2 = llvm::MemoryBuffer::getFileAsStream(f);
          VFS->addFile(f, mod_time, std::move(mb2.get()));
        }
      }

      //      auto e = llvm::sys::fs::rename("/home/tom/CloverLeaf", "/home/tom/CloverLeaf2");
      //      std::cout << e.message() << "\n";

      auto opt = std::make_shared<clang::HeaderSearchOptions>();
      opt->ModulesValidateOncePerBuildSession = true;
      auto that =
          ASTUnit::LoadFromASTFile((*r.pchName + "a"),               //
                                   clang::RawPCHContainerReader(),   //
                                   ASTUnit::WhatToLoad::LoadASTOnly, //
                                   diagnostics,                      //
                                   clang::FileSystemOptions(""),     //
                                   opt, false, true, CaptureDiagsKind::None, true, true, VFS);

      std::cout << that << "\n";
    }
  });

  return 0;

  //  ClangTool Tool(*that, files);
  //
  //  std::vector<std::unique_ptr<ASTUnit>> xs;
  //  if (auto result = Tool.buildASTs(xs); result != 0) {
  //    std::cout << "Build AST failed: " << result << "\n";
  //    return result;
  //  }
  //  std::cout << xs.size() << "\n";
  //  //
  //  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts2 = new DiagnosticOptions();
  //  auto diagnostics = CompilerInstance::createDiagnostics(DiagOpts2.get());
  //
  //  //  IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
  //  //  IntrusiveRefCntPtr<DiagnosticIDs> diagnosticIDs(new DiagnosticIDs());
  //  //  IntrusiveRefCntPtr<DiagnosticsEngine> diagnostics(
  //  //      new DiagnosticsEngine(diagnosticIDs, DiagOpts.get(),
  //  //                            new TextDiagnosticPrinter(llvm::errs(), DiagOpts.get()), true));
  //
  //  llvm::vfs::InMemoryFileSystem mem;
  //
  //  xs[0]->Save("foo.ast");
  //  //  auto opt = std::make_shared<clang::HeaderSearchOptions>();
  //  //  auto that = ASTUnit::LoadFromASTFile("foo.ast",                           //
  //  //                                       clang::RawPCHContainerReader(),      //
  //  //                                       ASTUnit::WhatToLoad::LoadEverything, //
  //  //                                       diagnostics,                         //
  //  //                                       clang::FileSystemOptions(""),        //
  //  //                                       opt);
  //  //  xs[0] = std::move(that);
  //
  //  std::cout << xs.size() << "\n";
  //
  //  //  using Label = label::StringLabel;
  //  //  using CostModelLD = cost_model::UnitCostModelLD<Label>;
  //  //  using LabelDictionary = label::LabelDictionary<Label>;
  //
  //  //  std::vector<node::Node<Label>> trees;
  //  for (auto &unit : xs) {
  //
  //    std::cout << unit->getMainFileName().str() << "\n";
  //
  //    auto keep = topLevelDeclsInMainFile(*unit);
  //
  //    keep | for_each([&](Decl *x) {
  //      if (auto fn = llvm::dyn_cast<FunctionDecl>(x); fn) {
  //        // No prototype, skip
  //        if (!fn->getPreviousDecl()) { return; }
  //      }
  //
  //      p3md::SemanticNode<std::string> root{"root", {}};
  //      p3md::TreeSemanticVisitor V(&root, unit->getASTContext(),
  //                                  p3md::TreeSemanticVisitor::Option{
  //                                      .normaliseVarName = false, //
  //                                      .normaliseFnName = false,  //
  //                                      .roots = options.rootDirs  //
  //                                  });
  //      V.TraverseDecl(x);
  //
  //      //      root.print(std::identity(), std::cout);
  //
  //      //      nlohmann::json a = root;
  //      //      a.dump(2);
  //
  //      //      clang::ASTWriter w();
  //
  //      //      std::cout << std::setw(2) << a << "\n";
  //
  //      x->dump();
  //    });
  //
  //    TSParser *parser = ts_parser_new();
  //    ts_parser_set_language(parser, tree_sitter_cpp());
  //    auto x = unit->getSourceManager().getBufferData(unit->getSourceManager().getMainFileID());
  //    const char *source_code = x.data();
  //    //    std::cout << source_code << "\n";
  //
  //    TSTree *tree = ts_parser_parse_string(parser, nullptr, source_code,
  //    std::strlen(source_code));
  //
  //    TSNode root_node = ts_tree_root_node(tree);
  //    //    walk<TSNode>(
  //    //        0, {}, root_node, [](const TSNode &n) { return std::string(ts_node_type(n)); },
  //    //        [&](const TSNode &n) -> std::vector<TSNode> {
  //    //          return iota<uint32_t>(0, ts_node_child_count(n))              //
  //    //                 | map([&](uint32_t i) { return ts_node_child(n, i); }) //
  //    //                 | to_vector();
  //    //        });
  //
  //    //    trees.emplace_back(makeTree(0, root_node));
  //
  //    //    std::cout << ts_node_child_count(root_node) << "\n";
  //
  //    //    char *string = ts_node_string(root_node);
  //    //    printf("Syntax tree: %s\n", string);
  //    //
  //    //    std::free(string);
  //    ts_tree_delete(tree);
  //    ts_parser_delete(parser);
  //  }
  //
  //  //  LabelDictionary ld;
  //  //  CostModelLD ucm(ld);
  //  //  ted::APTEDTreeIndex<CostModelLD, node::TreeIndexAPTED> apted_algorithm(ucm);
  //  //  node::TreeIndexAPTED ti1;
  //  //  node::TreeIndexAPTED ti2;
  //  //
  //  //  node::index_tree(ti1, trees[0], ld, ucm);
  //  //  node::index_tree(ti2, trees[1], ld, ucm);
  //  //  std::cout << "Distance TED:" << apted_algorithm.ted(ti1, ti2) << std::endl;
  //  //    return Tool.run(newFrontendActionFactory<clang::SyntaxOnlyAction>().get());
  //  return EXIT_SUCCESS;
}

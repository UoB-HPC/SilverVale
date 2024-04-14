#include <fstream>
#include <iostream>
#include <memory>
#include <system_error>
#include <thread>

#include "tree_sitter/api.h"
#include "tree_sitter_c/api.h"
#include "tree_sitter_cpp/api.h"
#include "tree_sitter_fortran/api.h"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

#include "aspartame/optional.hpp"
#include "aspartame/set.hpp"
#include "aspartame/unordered_map.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "p3md/build.h"
#include "p3md/compress.h"
#include "p3md/database.h"
#include "p3md/glob.h"

// #include "apted_tree_index.h"
// #include "node.h"
// #include "string_label.h"
// #include "unit_cost_model.h"

using namespace aspartame;
using namespace clang;
using namespace clang::tooling;


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

ArgumentsAdjuster p3md::build::Options::resolveAdjuster() const {
  return combineAdjusters(getInsertArgumentAdjuster(argsBefore, ArgumentInsertPosition::BEGIN),
                          getInsertArgumentAdjuster(argsAfter, ArgumentInsertPosition::END));
}

std::unique_ptr<CompilationDatabase>
p3md::build::Options::resolveDatabase(const ArgumentsAdjuster &adjuster) const {
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

// node::Node<label::StringLabel> makeTree(int depth, const TSTree &node) {
//   node::Node<label::StringLabel> n{{std::string(ts_node_type(node))}};
//   for (uint32_t i = 0; i < ts_node_child_count(node); ++i) {
//     n.add_child(makeTree(depth + 1, ts_node_child(node, i)));
//   }
//   return n;
// }

static int clearAndCreateDir(bool clear, const std::string &outDir) {
  auto dirOp = [&outDir](const std::string &op, auto f) {
    if (auto error = f(); error) {
      std::cerr << "Failed to " << op << " " << outDir << ": " << error.message() << "\n";
      return error.value();
    }
    return EXIT_SUCCESS;
  };

  auto code = EXIT_SUCCESS;
  if (clear) {
    code = dirOp("remove", [&]() { return llvm::sys::fs::remove_directories(outDir); });
    if (code != EXIT_SUCCESS) return code;
    code = dirOp("create", [&]() { return llvm::sys::fs::create_directories(outDir); });
    if (code != EXIT_SUCCESS) return code;
  } else {
    code = dirOp("create", [&]() { return llvm::sys::fs::create_directories(outDir); });
    if (code != EXIT_SUCCESS) return code;
    std::error_code error;
    auto begin = llvm::sys::fs::directory_iterator(outDir, error);
    if (error) {
      std::cerr << "Failed to traverse output directory " << outDir << ": " << error.message()
                << "\n";
      return error.value();
    }
    if (begin != llvm::sys::fs::directory_iterator()) {
      std::cerr << "Output directory " << outDir
                << " not empty, use --clear to clear output directory\n";
      return EXIT_FAILURE;
    }
  }
  return code;
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
    std::unordered_map<std::string, std::string> dependencies;
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
         //         auto stream = std::make_shared<zstd_ostream>(task.pchName, task.error);
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
          for (auto &task : tasks) {
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
            results[task.idx] = {task.sourceName, task.pchName, messageStorage, {}};
            auto &sm = out[0]->getSourceManager();
            std::for_each(sm.fileinfo_begin(), sm.fileinfo_end(), [&](auto entry) {
              if (auto name = entry.getFirst()->getName().str(); !name.empty()) {
                auto file = entry.getFirst()->tryGetRealPathName().str();
                results[task.idx].dependencies.emplace(name, file);
              }
            });
          }
        });
      });
  success.clear();
  for (auto &t : threads)
    t.join();

  std::cout << std::endl;

  success.clear(); // drop the streams so the file can close
  for (auto &t : failed)
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
  //  std::cout << "Roots:\n";
  //  for (auto &root : options.rootDirs)
  //    std::cout << " - " << root << "\n";

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
  auto files =
      db->getAllFiles() ^ filter([&](auto file) { return std::regex_match(file, regex); }) ^ sort();
  std::cout << "Adjustments: " << (adjuster({"${ARGS}"}, "${FILE}") | mk_string(" ")) << "\n";
  std::cout << "Sources (" << files.size() << "/" << db->getAllFiles().size() << "):\n";
  for (auto &file : files) {
    std::cout << " - " << file << "\n";
  }

  if (auto result = clearAndCreateDir(options.clearOutDir, options.outDir);
      result != EXIT_SUCCESS) {
    return result;
  }

  std::error_code error;
  auto dbFile =
      llvm::raw_fd_ostream(options.outDir + "/db.json", error, llvm::sys::fs::CD_CreateAlways);
  if (error) {
    std::cerr << "Cannot create db.json:" << error.message() << std::endl;
    return error.value();
  }

  auto unitsPerThread =
      static_cast<size_t>(std::ceil(static_cast<double>(files.size()) / options.maxThreads));

  std::cout << "Scheduling " << unitsPerThread << " unit(s) per thread for a total of "
            << options.maxThreads << " thread(s) to process " << files.size() << " entries."
            << std::endl;

  auto results = buildPCHParallel(*db, files, options.outDir, unitsPerThread);

  std::unordered_map<std::string, p3md::Database::Source> dependencies;
  for (auto &result : results) {
    for (auto &[_, file] : result.dependencies) {
      auto buffer = llvm::MemoryBuffer::getFile(file, /*isText*/ true);
      if (auto e = buffer.getError()) {
        std::cerr << "Cannot read file " << file << ": " << e.message() << std::endl;
        return e.value();
      }
      llvm::sys::fs::file_status status;
      if (auto e = llvm::sys::fs::status(file, status); e) {
        std::cerr << "Cannot stat file " << file << ": " << e.message() << std::endl;
        return e.value();
      }
      dependencies.emplace(
          file, p3md::Database::Source{llvm::sys::toTimeT(status.getLastModificationTime()),
                                       (*buffer)->getBuffer().str()});
    }
  }

  auto dbEntries =
      results | collect([&](auto &result) {
        return result.pchName ^ map([&](auto name) {
                 return std::pair{result.sourceName,
                                  p3md::Database::PCHEntry{
                                      db->getCompileCommands(result.sourceName) ^ map([](auto &cc) {
                                        return cc.CommandLine ^ mk_string(" ");
                                      }),
                                      name, "", result.dependencies

                                  }};
               });
      }) |
      and_then([](auto xs) {
        return std::unordered_map{xs.begin(), xs.end()};
      });

  auto totalSourceBytes = dependencies | values() | map([](auto x) { return x.content.size(); }) |
                          fold_left(0, std::plus<>());

  std::cout << "Database contains " << dependencies.size() << " dependent sources (total "
            << std::round(static_cast<double>(totalSourceBytes) / 1000 / 1000) << " MB)"
            << std::endl;

  nlohmann::json databaseJson = p3md::Database( //
      CLANG_VERSION_MAJOR, CLANG_VERSION_MINOR, CLANG_VERSION_PATCHLEVEL, dbEntries, dependencies);

  dbFile << databaseJson.dump(1);
  dbFile.close();

  return EXIT_SUCCESS;
}

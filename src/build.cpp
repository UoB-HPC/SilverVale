#include <fstream>
#include <iostream>
#include <memory>
#include <system_error>
#include <thread>

#include "p3md/build.h"
#include "p3md/compress.h"
#include "p3md/database.h"
#include "p3md/glob.h"
#include "p3md/par.h"
#include "p3md/term.h"

#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Frontend/TextDiagnosticPrinter.h"
#include "clang/Serialization/ASTReader.h"
#include "clang/Serialization/ASTWriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"

#include "aspartame/optional.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace clang;
using namespace clang::tooling;

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
      return AdjustingCompilations;
    }
    root = llvm::sys::path::parent_path(root);
  }
  return {};
}

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
  CompileCommand cmd;
  std::string pchName;

  std::error_code error;
  std::shared_ptr<raw_ostream> stream;
  struct Result {
    std::string sourceName;
    std::optional<std::string> pchName;
    std::vector<p3md::Database::Bitcode> bitcodes;
    std::string diagnostic;
    std::map<std::string, std::string> dependencies;
  };
};

static std::vector<Task::Result> buildPCHParallel(const std::string &root,
                                                  const CompilationDatabase &db,
                                                  const std::vector<CompileCommand> &commands,
                                                  std::string outDir, bool verbose,
                                                  bool noCompress) {

  auto [success, failed] =
      (commands | zip_with_index() | map([&](auto &cmd, auto idx) {
         Task task{.idx = idx,
                   .cmd = cmd,
                   .pchName = outDir + "/" + std::to_string(idx) + "." +
                              llvm::sys::path::filename(cmd.Filename).str() + ".pch" +
                              (noCompress ? "" : ".zstd"),
                   .error = std::make_error_code(std::errc()),
                   .stream = nullptr};
         if (noCompress) {
           task.stream = std::make_shared<llvm::raw_fd_stream>(task.pchName, task.error);
         } else {
           task.stream = std::make_shared<p3md::utils::zstd_ostream>(task.pchName, task.error, 6);
         }
         return task;
       }) |
       to_vector()) ^
      partition([](auto &t) { return t.error == std::errc(); });

  auto maxFileLength = commands ^ fold_left(0, [](auto acc, auto &s) {
                         return std::max(acc, static_cast<int>(s.Filename.size()));
                       });
  std::atomic_size_t completed{};
  std::vector<Task::Result> results(success.size());

  success | zip_with_index() | for_each([&](auto &task, auto idx) {
    auto tuName = llvm::sys::path::stem(task.cmd.Filename).str();
    auto searchPath = root + "/" + llvm::sys::path::parent_path(task.cmd.Output).str();

    // The BC file exists at the same level as .o when using -save-temps=obj, with the
    // following convention:
    // Host
    //    {OUTPUT}.bc
    // CUDA/HIP
    //   {OUTPUT}-host-{triple}.bc
    //   {OUTPUT}-{cuda|hip}-{triple}.bc
    // OpenMP target:
    //   {OUTPUT}-host-{triple}.bc
    //   {OUTPUT}-openmp-{triple}.bc

    auto copyAndSaveBC = [&](const std::string &src, const std::string &destName,
                             const std::string &kind, const std::string &triple) {
      if (auto copyEC = llvm::sys::fs::copy_file(src, outDir + "/" + destName); copyEC)
        std::cerr << "Warning: failed to copy BC (" + src << ") to " << (outDir + "/" + destName)
                  << ": " << copyEC.message() << " D=" << llvm::sys::fs::exists(outDir)
                  << std::endl;
      else { results[task.idx].bitcodes.emplace_back(destName, kind, triple); }
    };

    // First handle the common case where the direct bc exists
    if (auto hostBCFile = searchPath + "/" + tuName + ".bc"; llvm::sys::fs::exists(hostBCFile)) {
      copyAndSaveBC(hostBCFile, std::to_string(idx) + "." + tuName + ".bc", "", "");
    }

    // Then the offload and host ones
    std::regex pattern("^" + tuName + "-([a-zA-Z]+)-([a-zA-Z0-9-_]+)\\.bc$");
    std::error_code walkEC{};
    for (llvm::sys::fs::directory_iterator
             it = llvm::sys::fs::directory_iterator(searchPath, walkEC),
             itEnd;
         it != itEnd && !walkEC; it.increment(walkEC)) {
      std::smatch match;
      std::string source = llvm::sys::path::filename(it->path()).str();
      if (std::regex_match(source, match, pattern))
        copyAndSaveBC(it->path(), std::to_string(idx) + "." + source, match[1].str(),
                      match[2].str());
    }
    if (walkEC) {
      std::cerr << "Warning: failed to traverse -save-temps path " + searchPath << ": "
                << walkEC.message() << std::endl;
    }
  });

  p3md::par_for(success, [&, total = commands.size()](auto &task, auto idx) {
    auto compileCommand = [&]() {
      return (db.getCompileCommands(task.cmd.Filename) ^
              mk_string("\n", [](auto &cc) { return cc.CommandLine ^ mk_string(" "); }));
    };

    std::string messageStorage;
    llvm::raw_string_ostream message(messageStorage);

    IntrusiveRefCntPtr<DiagnosticOptions> DiagOpts = new DiagnosticOptions();
    TextDiagnosticPrinter diagPrinter(message, DiagOpts.get());

    ClangTool Tool(db, {task.cmd.Filename});

    Tool.setDiagnosticConsumer(&diagPrinter);
    std::vector<std::unique_ptr<ASTUnit>> out;
    auto result = Tool.buildASTs(out);
    diagPrinter.finish();
    if (result != 0) message << "# Clang-Tool exited with non-zero code: " << result << "\n";
    (P3MD_COUT << "[" << completed++ << "/" << total << "] " << std::left
               << std::setw(maxFileLength) << task.cmd.Filename << "\r")
        .flush();
    if (out.size() != 1)
      message << "# More than one AST unit produced; "
              << "input command is ill-formed and only the first unit will be preserved\n";

    if (out[0]->serialize(*task.stream)) // XXX true is fail
      message << "# Serialisation failed\n";
    results[task.idx].sourceName = task.cmd.Filename;
    results[task.idx].pchName = llvm::sys::path::filename(task.pchName).str();
    results[task.idx].diagnostic = messageStorage;
    auto &sm = out[0]->getSourceManager();
    std::for_each(sm.fileinfo_begin(), sm.fileinfo_end(), [&](auto entry) {
      if (auto name = entry.getFirst()->getName().str(); !name.empty()) {
        auto file = entry.getFirst()->tryGetRealPathName().str();
        results[task.idx].dependencies.emplace(name, file);
      }
    });
    if (!messageStorage.empty()) {
      if (verbose) (P3MD_COUT << messageStorage).flush(); // skip dump command
      else (P3MD_COUT << compileCommand() << "\n" << messageStorage).flush();
    } else {
      if (verbose) { P3MD_COUT << compileCommand() << std::endl; }
    }
  });

  success.clear();

  P3MD_COUT << std::endl;
  success.clear(); // drop the streams so the file can close
  for (auto &t : failed)
    results.emplace_back(t.cmd.Filename, std::nullopt, std::vector<p3md::Database::Bitcode>{},
                         t.error.message());
  return results;
}

int p3md::build::run(const p3md::build::Options &options) {

  P3MD_COUT //
      << "Build:\n"
      << " - Build:        " << options.buildDir << " (compile_commands.json, ...)\n"
      << " - Output:       " << options.outDir << "\n"
      << " - Clear output: " << (options.clearOutDir ? "true" : "false") << "\n"
      << " - Max threads:  " << options.maxThreads << "\n";

  auto global_limit = p3md::par_setup(options.maxThreads);

  auto adjuster = options.resolveAdjuster();

  if (options.clangResourceDir) {
    adjuster = combineAdjusters( //
        adjuster, getInsertArgumentAdjuster(
                      CommandLineArguments{
                          "-Xclang",
                          "-resource-dir",
                          "-Xclang",
                          *options.clangResourceDir,
                          "-Xclang",
                          "-internal-isystem",
                          "-Xclang",
                          *options.clangResourceDir,
                      },
                      ArgumentInsertPosition::END));
  }

  std::shared_ptr<CompilationDatabase> db = options.resolveDatabase(adjuster);
  if (!db) {
    std::cerr << "Unable to open compilation database at build dir `" << options.buildDir
              << "`, please check if compile_commands.json exists in that directory." << std::endl;
    return EXIT_FAILURE;
  }
  auto regexes = options.sourceGlobs ^ map([](auto &glob) { return globToRegex(glob); });

  auto commands =
      db->getAllCompileCommands() ^ filter([&](auto &cmd) {
        return regexes ^ exists([&](auto &r) { return std::regex_match(cmd.Filename, r); });
      }) ^
      sort_by([](auto &cmd) { return cmd.Filename; });

  P3MD_COUT << "Adjustments: " << (adjuster({"${ARGS}"}, "${FILE}") | mk_string(" ")) << "\n";
  P3MD_COUT << "Sources (" << commands.size() << "/" << db->getAllFiles().size() << "):\n";
  for (auto &cmd : commands) {
    P3MD_COUT << " - " << cmd.Filename << "\n";
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

  auto results = buildPCHParallel(options.buildDir, *db, commands, options.outDir, options.verbose,
                                  options.noCompress);

  std::map<std::string, p3md::Database::Dependency> dependencies;
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
          file, p3md::Database::Dependency{llvm::sys::toTimeT(status.getLastModificationTime()),
                                           (*buffer)->getBuffer().str()});
    }
  }

  auto dbEntries =
      results | collect([&](auto &result) {
        return result.pchName ^ map([&](auto name) {
                 return std::pair{result.sourceName,
                                  p3md::Database::Entry{
                                      .compileCommands = db->getCompileCommands(result.sourceName) ^
                                                         map([](auto &cc) {
                                                           return cc.CommandLine ^ mk_string(" ");
                                                         }),
                                      .pchName = name,
                                      .bitcodes = result.bitcodes,
                                      .dependencies = result.dependencies

                                  }};
               });
      }) |
      and_then([](auto xs) { return std::map{xs.begin(), xs.end()}; });

  auto totalSourceBytes = dependencies | values() | map([](auto &x) { return x.content.size(); }) |
                          fold_left(0, std::plus<>());

  P3MD_COUT << "Database contains " << dependencies.size() << " dependent sources (total="
            << std::round(static_cast<double>(totalSourceBytes) / 1000 / 1000) << " MB)"
            << std::endl;

  nlohmann::json databaseJson = p3md::Database( //
      CLANG_VERSION_MAJOR, CLANG_VERSION_MINOR, CLANG_VERSION_PATCHLEVEL, dbEntries, dependencies);

  dbFile << databaseJson.dump(1);
  dbFile.close();

  return EXIT_SUCCESS;
}

#include <fstream>
#include <iostream>
#include <memory>
#include <system_error>
#include <thread>

#include "agv/cli.h"
#include "agv/compress.h"
#include "agv/database.h"
#include "agv/glob.h"
#include "agv/par.h"
#include "agv/tool_index.h"

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
using namespace clang::tooling;
using namespace clang;
using namespace llvm;

ArgumentsAdjuster agv::index::Options::resolveAdjuster() const {
  return combineAdjusters(getInsertArgumentAdjuster(argsBefore, ArgumentInsertPosition::BEGIN),
                          getInsertArgumentAdjuster(argsAfter, ArgumentInsertPosition::END));
}

std::unique_ptr<CompilationDatabase>
agv::index::Options::resolveDatabase(const ArgumentsAdjuster &adjuster) const {
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
    std::vector<agv::Database::Bitcode> bitcodes;
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
           task.stream = std::make_shared<agv::utils::zstd_ostream>(task.pchName, task.error, 6);
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

  auto logger = agv::ProgressLogger(commands.size(), maxFileLength);
  agv::par_for(success, [&](auto &task, auto idx) {
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
    logger.log(task.cmd.Filename, false);
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
      if (verbose) (AGV_COUT << messageStorage).flush(); // skip dump command
      else (AGV_COUT << compileCommand() << "\n" << messageStorage).flush();
    } else {
      if (verbose) { AGV_COUT << compileCommand() << std::endl; }
    }
  });

  success.clear();

  AGV_COUT << std::endl;
  success.clear(); // drop the streams so the file can close
  for (auto &t : failed)
    results.emplace_back(t.cmd.Filename, std::nullopt, std::vector<agv::Database::Bitcode>{},
                         t.error.message());
  return results;
}

static llvm::Expected<agv::index::Options> parseOpts(int argc, const char **argv) {

  static cl::OptionCategory category("Build options");

  static cl::opt<std::string> buildDir(
      "build", cl::desc("The build directory containing compile_command.json."), //
      cl::cat(category));

  static cl::list<std::string> sourceGlobs( //
      cl::Positional, cl::ZeroOrMore,
      cl::desc("<glob patterns for file to include in the database, defaults to *>"),
      cl::list_init<std::string>({"*"}), cl::cat(category));

  static cl::opt<std::string> outDir(
      "out",
      cl::desc("The output directory for storing database files, "
               "defaults to the last 2 segments of the build directory joined with full stop."), //
      cl::init(""), cl::cat(category));

  static cl::list<std::string> argsAfter( //
      "args-before", cl::desc("Extra arguments to prepend to the compiler command line."),
      cl::cat(category));

  static cl::list<std::string> argsBefore( //
      "args-after", cl::desc("Extra arguments to append to the compiler command line."),
      cl::cat(category));

  static cl::opt<std::string> clangResourceDir(
      "resource-dir",
      cl::desc("Force the compiler to use a specific Clang resource directory (e.g path to "
               "`/usr/lib/clang/<VERSION>/include`); "
               "Use this if system headers such has <stddef.h> is not found."), //
      cl::init(""), cl::cat(category));

  static cl::opt<bool> clearOutDir( //
      "clear", cl::desc("Clear database output directory even if non-empty."), cl::cat(category));

  static cl::opt<bool> noCompress( //
      "no-compress", cl::desc("Compress individual entries in the database."), cl::cat(category));

  static cl::opt<bool> verbose( //
      "v",
      cl::desc("Print compile command line used for each translation unit; "
               "use -args-after=-v if you want to inspect detailed clang invocations."),
      cl::cat(category));

  static cl::opt<int> maxThreads( //
      "j",
      cl::desc(
          "Number of parallel AST frontend jobs in parallel, defaults to total number of threads."),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  if (auto e = agv::parseCategory(category, argc, argv); e) return std::move(*e);

  agv::index::Options options;
  options.buildDir = buildDir.getValue();
  options.sourceGlobs = sourceGlobs;
  options.argsBefore = argsBefore;
  options.argsAfter = argsAfter;
  options.clangResourceDir = clangResourceDir.getValue();

  options.clearOutDir = clearOutDir.getValue();
  options.maxThreads = maxThreads.getValue();
  options.verbose = verbose.getValue();
  options.noCompress = noCompress.getValue();

  if (outDir.empty()) {
    auto lastSegment = llvm::sys::path::filename(options.buildDir);
    auto oneBeforeLastSegment =
        llvm::sys::path::filename(llvm::sys::path::parent_path(options.buildDir));
    options.outDir = (oneBeforeLastSegment + "." + lastSegment).str();
  } else options.outDir = outDir.getValue();
  return options;
}

int agv::index::main(int argc, const char **argv) {
  return agv::parseAndRun(argc, argv, &parseOpts, &run);
}

int agv::index::run(const agv::index::Options &options) {

  AGV_COUT //
      << "Build:\n"
      << " - Build:        " << options.buildDir << " (compile_commands.json, ...)\n"
      << " - Output:       " << options.outDir << "\n"
      << " - Clear output: " << (options.clearOutDir ? "true" : "false") << "\n"
      << " - Max threads:  " << options.maxThreads << "\n";

  auto global_limit = par_setup(options.maxThreads);
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
              << "`, please check if compile_commands.json exists in that directory.\n"
              << "If you are using CMake, add `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON` and also set "
                 "the environment variable `CXXFLAGS=-save-temps=obj` (replace CXXFLAGS with "
                 "CUDAFLAGS for CUDA) for LLVM IR trees.\n"
                 "For example: \n"
                 "> CXXFLAGS=-save-temps=obj cmake -Bbuild -S <source_dir> "
                 "-DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_CXX_LINK_EXECUTABLE='echo \"\"'\n"
                 "Here we also disable linking entirely by setting the link command to simply echo "
                 "an empty string."
              << std::endl;
    return EXIT_FAILURE;
  }
  auto regexes = options.sourceGlobs ^ map([](auto &glob) { return globToRegex(glob); });

  auto commands =
      db->getAllCompileCommands() ^ filter([&](auto &cmd) {
        return regexes ^ exists([&](auto &r) { return std::regex_match(cmd.Filename, r); });
      }) ^
      sort_by([](auto &cmd) { return cmd.Filename; });

  AGV_COUT << "Adjustments: " << (adjuster({"${ARGS}"}, "${FILE}") | mk_string(" ")) << "\n";
  AGV_COUT << "Sources (" << commands.size() << "/" << db->getAllFiles().size() << "):\n";
  for (auto &cmd : commands) {
    AGV_COUT << " - " << cmd.Filename << "\n";
  }

  if (auto result = clearAndCreateDir(options.clearOutDir, options.outDir);
      result != EXIT_SUCCESS) {
    return result;
  }

  llvm::SmallVector<char> absOutDir(options.outDir.begin(), options.outDir.end());
  if (auto err = llvm::sys::fs::make_absolute(absOutDir); err) {
    std::cerr << "Cannot resolve absolute path for output dir " << options.outDir << ": "
              << err.message() << std::endl;
  }

  auto dbFile = options.outDir + "/db.json";
  std::ofstream dbStream(dbFile, std::ios::trunc);
  if (!dbStream) {
    std::cerr << "Cannot open " << dbFile << " for writing!" << std::endl;
    return EXIT_FAILURE;
  }

  // XXX Do ALL IO related to creating FBs before PCH! Otherwise ClangTool prevents new FD creation
  auto results = buildPCHParallel(options.buildDir, *db, commands, options.outDir, options.verbose,
                                  options.noCompress);

  std::map<std::string, Database::Dependency> dependencies;
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
          file, Database::Dependency{llvm::sys::toTimeT(status.getLastModificationTime()),
                                     (*buffer)->getBuffer().str()});
    }
  }

  auto dbEntries =
      results | collect([&](auto &result) {
        return result.pchName ^ map([&](auto name) {
                 return std::pair{
                     result.sourceName,
                     Database::Entry{.compileCommands = db->getCompileCommands(result.sourceName) ^
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

  AGV_COUT << "Database contains " << dependencies.size() << " dependent sources (total="
            << std::round(static_cast<double>(totalSourceBytes) / 1000 / 1000) << " MB)"
            << std::endl;

  nlohmann::json databaseJson = Database(                               //
      {{"clangMajorVersion", std::to_string(CLANG_VERSION_MAJOR)},      //
       {"clangMinorVersion", std::to_string(CLANG_VERSION_MINOR)},      //
       {"clangPatchVersion", std::to_string(CLANG_VERSION_PATCHLEVEL)}} //
      ,
      std::string(absOutDir.begin(), absOutDir.end()), dbEntries, dependencies);

  dbStream << databaseJson;
  return EXIT_SUCCESS;
}

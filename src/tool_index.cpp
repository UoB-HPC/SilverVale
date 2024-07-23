#include <filesystem>
#include <iostream>
#include <system_error>
#include <thread>

#include "sv/cli.h"
#include "sv/compress.h"
#include "sv/glob.h"
#include "sv/index_gcc.h"
#include "sv/index_llvm.h"
#include "sv/par.h"
#include "sv/tool_index.h"

#include "clang/Tooling/CommonOptionsParser.h"

#include "llvm/Support/Program.h"

#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace std::string_literals;
using namespace aspartame;
using namespace llvm;

std::unique_ptr<sv::CompilationDatabase> sv::index::Options::resolveDatabase() const {
  auto root = buildDir;
  while (!root.empty()) {
    std::ifstream s(root / "compile_commands.json");
    if (!s) root = root.parent_path();
    else {
      auto db = std::make_unique<CompilationDatabase>();
      nlohmann::from_json(nlohmann::json::parse(s), *db);
      return db;
    }
  }
  return {};
}

static int clearAndCreateDir(bool clear, const std::filesystem::path &outDir) {
  try {
    std::filesystem::create_directories(outDir);
    if (clear) {
      for (const auto &entry : std::filesystem::directory_iterator(outDir))
        std::filesystem::remove_all(entry.path());
    } else if (!std::filesystem::is_empty(outDir)) {
      AGV_ERRF("Output directory {} not empty, use --clear to clear output directory", outDir);
      return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
  } catch (const std::exception &e) {
    AGV_ERRF("Cannot determine output directory ({}) state:", outDir, e);
    return EXIT_FAILURE;
  }
}

static void runIndexTasks(const std::vector<sv::CompilationDatabase::Entry> &commands,
                          const std::filesystem::path &outDir, bool verbose) {

  std::error_code wdEC;
  auto currentPath = std::filesystem::current_path(wdEC);
  if (wdEC) {
    AGV_WARNF("cannot get current working directory ({}), index cannot proceed", wdEC);
    return;
  }

  // Resolve all non-absolute programs first
  std::unordered_map<std::string, std::string> programLUT;
  for (auto &cmd : commands) {
    auto program = cmd.command[0];
    if (!std::filesystem::path(program).is_absolute() && !programLUT.contains(program)) {
      auto name = sys::findProgramByName(program);
      if (auto e = name.getError()) {
        AGV_WARNF("cannot resolve program `{}`: {}", program, e.message());
      } else {
        AGV_INFOF("program {} resolves to {}", program, *name);
        programLUT.emplace(program, *name);
      }
    }
  }
  auto maxFileLength = commands ^ fold_left(0, [](auto acc, auto &cmd) {
                         return std::max(acc, static_cast<int>(cmd.file.size()));
                       });
  auto logger = sv::ProgressLogger(commands.size(), maxFileLength);
  for (auto &[wd, tasks] : commands ^ group_by([](auto &cmd) { return cmd.directory; })) {

    try {
      std::filesystem::current_path(wd);
    } catch (const std::exception &e) {
      AGV_WARNF("cannot change working directory for {} tasks: {}; skipping...", tasks.size(), e);
      continue;
    }

    AGV_INFOF("cd {}", wd);
    sv::par_for(tasks, [&](const sv::CompilationDatabase::Entry &cmd, auto idx) {
      if (cmd.command.empty()) {
        AGV_WARNF("empty command line for file {}", cmd.file);
        return;
      }
      logger.log(cmd.file);
      if (!sv::detectClangAndIndex(verbose, cmd, wd, outDir, programLUT)) {
        if (!sv::detectGccAndIndex(verbose, cmd, wd, outDir, programLUT)) {
          AGV_WARNF("unsupported compiler for {}", cmd.command ^ mk_string(" "));
        }
      }
    });
  }
  AGV_COUT << std::endl;
  try {
    std::filesystem::current_path(currentPath);
  } catch (const std::exception &e) {
    AGV_ERRF("Cannot restore current working directory to {}: {}", currentPath, e);
    return;
  }
}

static Expected<sv::index::Options> parseOpts(int argc, const char **argv) {

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

  static cl::opt<bool> clearOutDir( //
      "clear", cl::desc("Clear database output directory even if non-empty."), cl::cat(category));

  static cl::opt<bool> verbose( //
      "v", cl::desc("Print compile command line used for each translation unit."),
      cl::cat(category));

  static cl::opt<int> maxThreads( //
      "j", cl::desc("Number of jobs in parallel, defaults to total number of threads."),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  if (auto e = sv::parseCategory(category, argc, argv); e) return std::move(*e);

  sv::index::Options options;
  options.buildDir = buildDir.getValue();
  options.sourceGlobs = sourceGlobs;

  options.clearOutDir = clearOutDir.getValue();
  options.maxThreads = maxThreads.getValue();
  options.verbose = verbose.getValue();

  if (outDir.empty()) {
    auto lastSegment = options.buildDir.filename();
    auto oneBeforeLastSegment = options.buildDir.parent_path().filename();
    options.outDir = fmt::format("{}.{}", oneBeforeLastSegment, lastSegment);
  } else options.outDir = outDir.getValue();
  return options;
}

int sv::index::main(int argc, const char **argv) {
  return sv::parseAndRun(argc, argv, &parseOpts, &run);
}

int sv::index::run(const sv::index::Options &options) {

  AGV_COUT //
      << "Build:\n"
      << " - Build:        " << options.buildDir << " (compile_commands.json, ...)\n"
      << " - Output:       " << options.outDir << "\n"
      << " - Clear output: " << (options.clearOutDir ? "true" : "false") << "\n"
      << " - Max threads:  " << options.maxThreads << "\n";

  auto global_limit = par_setup(options.maxThreads);
  std::shared_ptr<CompilationDatabase> db = options.resolveDatabase();
  if (!db) {
    AGV_CERR << "Unable to open compilation database at build dir `" << options.buildDir
             << "`, please check if compile_commands.json exists in that directory.\n"
             << "If you are using CMake, add `-DCMAKE_EXPORT_COMPILE_COMMANDS=ON`.\n"
                "For example: \n"
                "> cmake -Bbuild -S <source_dir> -DCMAKE_EXPORT_COMPILE_COMMANDS=ON "
                "-DCMAKE_CXX_LINK_EXECUTABLE='echo \"\"'\n"
                "Here we also disable linking entirely by setting the link command to simply echo "
                "an empty string."
             << std::endl;
    return EXIT_FAILURE;
  }
  auto regexes = options.sourceGlobs ^ map([](auto &glob) { return globToRegex(glob); });

  auto commands = db->entries ^ filter([&](auto &cmd) {
                    return regexes ^ exists([&](auto &r) { return std::regex_match(cmd.file, r); });
                  }) ^
                  sort_by([](auto &cmd) { return cmd.file; });

  AGV_COUT << "Sources (" << commands.size() << "/" << db->entries.size() << "):\n";
  for (auto &cmd : commands) {
    AGV_COUT << " - " << cmd.file << "\n";
  }

  if (auto result = clearAndCreateDir(options.clearOutDir, options.outDir);
      result != EXIT_SUCCESS) {
    return result;
  }

  std::error_code ec;
  if (auto abs = std::filesystem::absolute(options.outDir, ec); ec) {
    AGV_ERRF("Cannot resolve absolute path for output dir {}: {}", options.outDir, ec);
  } else {
    runIndexTasks(commands, abs, options.verbose);
  }

  return EXIT_SUCCESS;
}

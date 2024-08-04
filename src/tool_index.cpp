#include <cxxopts.hpp>
#include <filesystem>
#include <iostream>
#include <system_error>
#include <thread>

#include "sv/cli.h"
#include "sv/exec.h"
#include "sv/glob.h"
#include "sv/index_gcc.h"
#include "sv/index_llvm.h"
#include "sv/par.h"
#include "sv/tool_index.h"

#include "aspartame/string.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/optional.hpp"
#include "aspartame/vector.hpp"
#include "zlib.h"

using namespace aspartame;

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
    }
    return EXIT_SUCCESS;
  } catch (const std::exception &e) {
    SV_ERRF("Cannot determine output directory ({}) state: {}", outDir, e);
    return EXIT_FAILURE;
  }
}

static void runCoverageTask(const std::filesystem::path &coverageBin,
                            const std::filesystem::path &coverageRawDir,
                            sv::index::CoverageKind kind, const std::filesystem::path &outDir,
                            bool verbose) {

  if (coverageBin.empty()) return;
  auto absCoverageBin = std::filesystem::absolute(coverageBin);
  if (absCoverageBin.empty()) {
    SV_INFOF("No coverage binary specified, no coverage data will be included");
    if (!coverageRawDir.empty()) {
      SV_WARNF("Ignoring coverage directory {} as no binary was specified", coverageRawDir);
    }
    return;
  }

  if (!std::filesystem::is_regular_file(absCoverageBin)) {
    SV_WARNF("Coverage binary {} not found or is not a file", absCoverageBin);
    return;
  }

  const auto exec = [&](const std::string &args, std::ostream &out) {
    if (verbose) SV_INFOF("{}", args);
    //    std::stringstream buffer;
    if (auto code = sv::exec(args, out); code) {
      if (*code != 0) SV_WARNF("non-zero return for `{}`", args);
    } else SV_WARNF("popen failed for `{}`: ", args);
    //    return buffer.str();
  };

  const auto clangSBCC = [&](auto &profrawFiles) {
    for (auto &p : profrawFiles)
      SV_INFOF("adding Clang SBCC coverage data: {}", p);

    auto profdataFile = std::filesystem::absolute(outDir / "default.profdata");
    exec(fmt::format("llvm-profdata merge -sparse {} -o {}", profrawFiles ^ mk_string(" "),
                     profdataFile),
         std::cout);

    auto output = outDir / sv::EntryClangSBCCName;
    {
      std::ofstream stream(output);
      exec(fmt::format("llvm-cov export {} -instr-profile={}", absCoverageBin, profdataFile),
           stream);
    }
    SV_INFOF("exported Clang SBCC coverage: {}", output);
  };

  const auto gcov = [&](auto gcovFiles) {
    std::error_code wdEC;
    auto currentPath = std::filesystem::current_path(wdEC);
    if (wdEC) {
      SV_ERRF("Cannot get current working directory ({}), gcov cannot proceed", wdEC);
      return;
    }

    for (auto &gcovFile : gcovFiles) {
      SV_INFOF("adding GCC GCov coverage data: {}", gcovFile);

      auto gcovWd = gcovFile.parent_path();
      try {
        std::filesystem::current_path(gcovWd);
      } catch (const std::exception &e) {
        SV_WARNF("Cannot set current directory to {} for gcov file {}, skipping coverage...",
                 gcovWd, gcovFile);
        return;
      }

      std::stringstream gcovOutcome;
      exec(fmt::format("gcov {} --json-format", gcovFile.filename()), gcovOutcome);

      if (gcovOutcome.str() ^ contains_slice("stamp mismatch with notes file")) {
        // this means the GCDA file didn't match up with GCNO and all coverage will be 0%
        SV_WARNF("stamp mismatch between gcov notes (*.gcda) and profile (*.gcda), please rerun "
                 "binary, skipping coverage...");
        return;
      }
      std::cout <<gcovOutcome .str()<<std::endl;

      auto output = outDir / fmt::format("{}.{}", gcovFile.stem(), sv::EntryGCCGCovName);

      {
        auto gcovOutput = fmt::format("{}.gcov.json.gz", gcovFile.stem());
        gzFile gzFile = gzopen(gcovOutput.c_str(), "rb");
        if (!gzFile) {
          SV_WARNF("cannot open {} for decompression, did gcov succeed?", gcovOutput);
          return;
        }
        std::ofstream stream(output);
        if (!stream) {
          SV_WARNF("cannot open {} for writing", output);
          gzclose(gzFile);
          return;
        }
        const int bufferSize = 4096;
        std::vector<char> buffer(bufferSize);
        int bytesRead{};
        while ((bytesRead = gzread(gzFile, buffer.data(), bufferSize)) > 0)
          stream.write(buffer.data(), bytesRead);

        gzclose(gzFile);
      }
      SV_INFOF("exported GCC GCov coverage: {}", output);
    }

    try {
      std::filesystem::current_path(currentPath);
    } catch (const std::exception &e) {
      SV_ERRF("Cannot restore current working directory to {}: {}", currentPath, e);
      return;
    }
  };

  std::vector<std::filesystem::path> covFiles;
  auto collectCovFiles = [&](auto root) {
    if (verbose) SV_INFOF("searching for coverage data in {}", root);
    for (const auto &entry : std::filesystem::recursive_directory_iterator(root)) {
      covFiles.emplace_back(std::filesystem::absolute(entry.path()));
    }
  };

  collectCovFiles(absCoverageBin.parent_path());
  if (!coverageRawDir.empty()) collectCovFiles(coverageRawDir);
  covFiles = covFiles ^ distinct();

  auto profRawFiles = covFiles ^ filter([](auto &p) { return p.extension() == ".profraw"; });
  auto gcovFiles = covFiles ^ filter([](auto &p) { return p.extension() == ".gcda"; });

  if (profRawFiles.empty() && gcovFiles.empty()) {
    SV_WARNF("not coverage data found");
    return;
  }

  switch (kind) {
    case sv::index::CoverageKind::AutoDetect:
      if (!profRawFiles.empty() && !gcovFiles.empty()) {
        SV_WARNF("Detected both Clang *.profraw and GCC *.gcda files [{},{}], please "
                 "specify with format should be used, skipping coverage... ",
                  profRawFiles ^ mk_string(","), gcovFiles ^ mk_string(","));
      }
      if (!profRawFiles.empty()) clangSBCC(profRawFiles);
      if (!gcovFiles.empty()) gcov(gcovFiles);
      break;
    case sv::index::CoverageKind::ClangSBCC: clangSBCC(profRawFiles); break;
    case sv::index::CoverageKind::GCov: gcov(gcovFiles); break;
  }
}

static void runIndexTasks(const std::vector<sv::CompilationDatabase::Entry> &commands,
                          const std::filesystem::path &outDir, bool verbose) {

  setenv("CCACHE_DISABLE", "1", true);

  std::error_code outEC;
  auto absOutDir = std::filesystem::absolute(outDir, outEC);
  if (outEC) {
    SV_ERRF("Cannot resolve absolute path for output dir {}: {}", outDir, outEC);
    return;
  }

  std::error_code wdEC;
  auto currentPath = std::filesystem::current_path(wdEC);
  if (wdEC) {
    SV_ERRF("Cannot get current working directory ({}), index cannot proceed", wdEC);
    return;
  }

  // Resolve all non-absolute programs first
  std::unordered_map<std::string, std::string> programLUT;
  for (auto &cmd : commands) {
    auto program = cmd.command[0];
    if (!std::filesystem::path(program).is_absolute() && !programLUT.contains(program)) {
      if (auto name = sv::findProgramByName(program); name) {
        SV_INFOF("program {} resolves to {}", program, *name);
        programLUT.emplace(program, *name);
      } else SV_WARNF("cannot resolve program `{}` from PATH", program);
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
      SV_WARNF("cannot change working directory for {} tasks: {}; skipping...", tasks.size(), e);
      continue;
    }

    SV_INFOF("cd {}", wd);
    sv::par_for(tasks, [&](const sv::CompilationDatabase::Entry &cmd, auto) {
      if (cmd.command.empty()) {
        SV_WARNF("empty command line for file {}", cmd.file);
        return;
      }
      logger.log(cmd.file);

      if (!sv::detectClangAndIndex(verbose, cmd, wd, absOutDir, programLUT)) {
        if (!sv::detectGccAndIndex(verbose, cmd, wd, absOutDir, programLUT)) {
          SV_WARNF("unsupported compiler in command `{}`", cmd.command ^ mk_string(" "));
        }
      }
    });
  }
  SV_COUT << std::endl;
  try {
    std::filesystem::current_path(currentPath);
  } catch (const std::exception &e) {
    SV_ERRF("Cannot restore current working directory to {}: {}", currentPath, e);
    return;
  }
}

int sv::index::main(int argc, const char **argv) {
  cxxopts::Options options(Name, Description);
  options.add_options() //
      ("build", "The build directory containing compile_command.json.",
       cxxopts::value<std::string>()) //
      ("out", "The output directory for storing database files",
       cxxopts::value<std::string>()) //
      ("sourceGlobs", "Glob patterns for file to include in the database, defaults to *",
       cxxopts::value<std::vector<std::string>>()->default_value({"*"})) //
      ("cov-bin",
       "Path to the binary compiled with coverage enabled. Profile data (*.profraw) files will "
       "be searched in the directory containing the binary; all discovered raw profiles will be "
       "merged.",
       cxxopts::value<std::string>()->default_value("")) //
      ("cov-raw",
       "Additional path to directory containing profile data (*.profraw) files; all discovered "
       "raw profiles will be merged.",
       cxxopts::value<std::string>()->default_value("")) //
      ("cov-kind", "Coverage data format, defaults to auto detect based on extension.",
       cxxopts::value<std::string>()->default_value("auto")) //
      ("clear", "Clear database output directory even if non-empty.",
       cxxopts::value<bool>()->default_value("false")) //
      ("v,verbose", "Print compile command line used for each translation unit.",
       cxxopts::value<bool>()->default_value("false")) //
      ("j,threads", "Number of jobs in parallel, defaults to total number of threads.",
       cxxopts::value<int>()->default_value(std::to_string(std::thread::hardware_concurrency())));

  options.parse_positional({"sourceGlobs"});
  auto result = options.parse(argc, argv);
  if (result.count("help")) {
    SV_COUT << options.help() << std::endl;
    return EXIT_SUCCESS;
  } else
    return run(Options{
        .buildDir = result["build"].as<std::string>(),
        .sourceGlobs = result["sourceGlobs"].as<std::vector<std::string>>(),
        .outDir = result["out"].as<std::string>(),
        .coverageBin = result["cov-bin"].as<std::string>(),
        .coverageRawDir = result["cov-raw"].as<std::string>(),
        .coverageKind = parseCoverageKind(result["cov-kind"].as<std::string>()) ^ fold([]() {
                          SV_WARNF("Cannot parse cov-kind, using default");
                          return CoverageKind::AutoDetect;
                        }),
        .clearOutDir = result["clear"].as<bool>(),
        .verbose = result["verbose"].as<bool>(),
        .maxThreads = result["threads"].as<int>(),

    });
}

int sv::index::run(const sv::index::Options &options) {

  SV_COUT //
      << "Build:\n"
      << " - Build:        " << options.buildDir << " (/compile_commands.json, ...)\n"
      << " - Coverage:     " << options.coverageBin << " (bin, bin/../*.profraw, ...)\n"
      << "   - Dir:        " << options.coverageRawDir << " (/*.profraw, ...)\n"
      << " - Output:       " << options.outDir << "\n"
      << " - Clear output: " << (options.clearOutDir ? "true" : "false") << "\n"
      << " - Max threads:  " << options.maxThreads << "\n";

  auto global_limit = par_setup(options.maxThreads);
  std::shared_ptr<CompilationDatabase> db = options.resolveDatabase();
  if (!db) {
    SV_CERR << "Unable to open compilation database at build dir `" << options.buildDir
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

  SV_COUT << "Sources (" << commands.size() << "/" << db->entries.size() << "):\n";
  for (auto &cmd : commands) {
    SV_COUT << " - " << cmd.file << "\n";
  }

  if (auto result = clearAndCreateDir(options.clearOutDir, options.outDir);
      result != EXIT_SUCCESS) {
    return result;
  }

  runCoverageTask(options.coverageBin, options.coverageRawDir, options.coverageKind, options.outDir,
                  options.verbose);
  runIndexTasks(commands, options.outDir, options.verbose);

  return EXIT_SUCCESS;
}

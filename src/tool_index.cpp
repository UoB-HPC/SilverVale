#include <fstream>
#include <iostream>
#include <memory>
#include <system_error>
#include <thread>

#include "agv/cli.h"
#include "agv/compress.h"
#include "agv/glob.h"
#include "agv/model.h"
#include "agv/par.h"
#include "agv/tool_index.h"

#include "fmt/core.h"

#include "clang/Basic/Version.h"
#include "clang/Driver/OffloadBundler.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "llvm/BinaryFormat/Magic.h"
#include "llvm/Object/OffloadBinary.h"
#include "llvm/Support/Program.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/variant.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace std::string_literals;
using namespace aspartame;
using namespace clang::tooling;
using namespace llvm;

#define AGV_WARNF(...) (AGV_CERR << fmt::format("# Warning: " __VA_ARGS__) << std::endl)
#define AGV_INFOF(...) (AGV_COUT << fmt::format("# " __VA_ARGS__) << std::endl)
#define AGV_ERRF(...) (AGV_CERR << fmt::format(__VA_ARGS__) << std::endl)

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
    root = sys::path::parent_path(root);
  }
  return {};
}

static int clearAndCreateDir(bool clear, const std::string &outDir) {
  auto dirOp = [&outDir](const std::string &op, auto f) {
    if (auto e = f(); e) {
      AGV_ERRF("Failed to {}: {}", outDir, e.message());
      return e.value();
    }
    return EXIT_SUCCESS;
  };
  auto code = EXIT_SUCCESS;
  if (clear) {
    code = dirOp("remove", [&]() { return sys::fs::remove_directories(outDir); });
    if (code != EXIT_SUCCESS) return code;
    code = dirOp("create", [&]() { return sys::fs::create_directories(outDir); });
    if (code != EXIT_SUCCESS) return code;
  } else {
    code = dirOp("create", [&]() { return sys::fs::create_directories(outDir); });
    if (code != EXIT_SUCCESS) return code;
    std::error_code e;
    auto begin = sys::fs::directory_iterator(outDir, e);
    if (e) {
      AGV_ERRF("Failed to traverse output directory {}: {}", outDir, e.message());
      return e.value();
    }
    if (begin != sys::fs::directory_iterator()) {
      AGV_ERRF("Output directory {} not empty, use --clear to clear output directory", outDir);
      return EXIT_FAILURE;
    }
  }
  return code;
}

// Clean-room of https://clang.llvm.org/docs/ClangOffloadBundler.html
// We can't use clang::OffloadBundler::ListBundleIDsInFile because it writes to llvm::outs()
// and we can't easily capture the output when threaded.
struct ClangOffloadBundle {
  static constexpr char BundleMagic[] = "__CLANG_OFFLOAD_BUNDLE__";
  struct Entry {
    uint64_t offset, size, idLength;
    std::string id;
  };
  std::vector<Entry> entries;
  static std::optional<Expected<ClangOffloadBundle>> parse(const std::string &filename) {
    auto fail = [](auto &&s) { return make_error<StringError>(s, inconvertibleErrorCode()); };
    std::ifstream file(filename, std::ios::binary);
    if (!file) return fail(fmt::format("Cannot open {}", filename));

    char magic[sizeof(BundleMagic) - 1];
    if (!(file.read(magic, sizeof(magic)) &&
          (std::strncmp(magic, BundleMagic, sizeof(magic)) == 0))) {
      return {};
    }

    auto read = [&]<typename T>(T *field, auto name) -> std::optional<std::string> {
      if (!file.read(reinterpret_cast<char *>(field), sizeof(T))) {
        return (fmt::format("Cannot read {} bytes for ClangOffloadBundle.{}", sizeof(T), name));
      }
      return {};
    };
    ClangOffloadBundle bundle;
    uint64_t numEntries{};
    if (auto e = read(&numEntries, "numEntries"); e) return fail(*e);
    for (uint64_t i = 0; i < numEntries; ++i) {
      ClangOffloadBundle::Entry entry{};
      if (auto e = read(&entry.offset, "offset"); e) { return fail(*e); }
      if (auto e = read(&entry.size, "size"); e) { return fail(*e); }
      if (auto e = read(&entry.idLength, "idLength"); e) { return fail(*e); }
      entry.id.resize(entry.idLength);
      if (!file.read(entry.id.data(), static_cast<std::streamsize>(entry.idLength))) {
        return fail(fmt::format("Cannot read {} bytes for ClangOffloadBundle.id", entry.idLength));
      } else bundle.entries.push_back(entry);
    }
    return bundle;
  }
};

// Handle TU.bc/TU-$triple.bc bitcode file
// -save-temps has the following convention for BC temps:
//   Host
//      {OUTPUT}.bc
//   CUDA/HIP
//     {OUTPUT}-host-{triple}.bc
//     {OUTPUT}-{cuda|hip}-{triple}.bc
//   OpenMP target:
//     {OUTPUT}-host-{triple}.bc
//     {OUTPUT}-openmp-{triple}.bc
// -emit-llvm uses the same format where:
//   Only CUDA will create a BC file,
//   HIP generates a clang-offload-bundle file
//   OpenMP target generates a normal BC with a 0x10ff10ad prefixed @llvm.embedded.object
//   see https://clang.llvm.org/docs/ClangOffloadPackager.html
static std::vector<agv::ClangDatabase::Bitcode> collectBitcodeFiles(bool verbose, size_t idx,
                                                                    const std::string name,
                                                                    const std::string &wd,
                                                                    const std::string &dest) {
  std::vector<agv::ClangDatabase::Bitcode> codes;
  auto saveBC = [&, pattern = std::regex("^" + name + "-([a-zA-Z]+)-([a-zA-Z0-9-_]+)\\.bc$")](
                    const std::string &src, const std::string &dest) {
    if (auto e = sys::fs::copy_file(src, dest); e)
      AGV_WARNF("failed to copy BC {} to {}: {}", src, dest, e.message());
    else {
      auto buffer = MemoryBuffer::getFile(src);
      if (!buffer) {
        AGV_WARNF("error reading BC {}: {}", src, buffer.getError().message());
        return;
      }
      auto bufferRef = buffer->get()->getMemBufferRef();
      if (auto magic = identify_magic(bufferRef.getBuffer()); magic != file_magic::bitcode) {
        AGV_WARNF("file {} is not a BC file (llvm::file_magic index={})", src,
                  static_cast<std::underlying_type_t<file_magic::Impl>>(magic));
        return;
      }

      SmallVector<object::OffloadFile> binaries;
      if (auto _ = object::extractOffloadBinaries(bufferRef, binaries)) {
        AGV_WARNF("error reading embedded offload binaries for {}", src);
      }
      binaries | filter([](auto &f) {
        return f.getBinary()->getImageKind() == object::ImageKind::IMG_Bitcode;
      }) | for_each([&](auto &f) {
        auto kind = getOffloadKindName(f.getBinary()->getOffloadKind()).str();
        auto triple = f.getBinary()->getTriple().str();
        auto embeddedName = fmt::format("{}.{}-{}-{}.bc", idx, name, kind, triple);
        if (verbose)
          AGV_INFOF("adding embedded BC {} (kind={}, triple={})", embeddedName, kind, triple);
        std::error_code embeddedEC;
        llvm::raw_fd_ostream file(
            fmt::format("{}/{}", sys::path::parent_path(dest).str(), embeddedName), embeddedEC);
        file << f.getBinary()->getImage();
        if (embeddedEC) {
          AGV_WARNF("failed to write embedded offload binary {}: {}", embeddedName,
                    embeddedEC.message());
        } else codes.emplace_back(embeddedName, kind, triple);
      });

      auto destName = sys::path::filename(dest).str();
      std::smatch match;
      auto [_, kind, triple] = std::regex_match(destName, match, pattern)
                                   ? codes.emplace_back(destName, match[1].str(), match[2].str())
                                   : codes.emplace_back(destName, "host", "");
      if (verbose) AGV_INFOF("adding BC: {} (kind={}, triple={})", destName, kind, triple);
    }
  };

  // first walk the wd to discover any existing target BC
  std::error_code walkError{};
  for (sys::fs::directory_iterator it = sys::fs::directory_iterator(wd, walkError), itEnd;
       it != itEnd && !walkError; it.increment(walkError)) {
    std::string bcFile = sys::path::filename(it->path()).str();
    if (bcFile ^ starts_with(name + "-") && bcFile ^ ends_with(".bc")) {
      saveBC(bcFile, fmt::format("{}/{}.{}", dest, idx, bcFile));
    }
  }

  if (walkError)
    AGV_WARNF("failed to traverse working directory {} for BC files:{} ", wd, walkError.message());

  // then handle the host BC itself
  std::string hostBCFile;
  if (auto bcFile = fmt::format("{}/{}.bc", wd, name); sys::fs::exists(bcFile))
    hostBCFile = bcFile; //
  else if (auto oFile = fmt::format("{}/{}.o", wd, name); sys::fs::exists(oFile))
    hostBCFile = oFile; //
  if (!hostBCFile.empty()) {
    // found a valid host BC, it could be a clang offload bundle: try to unbundle
    // The following drivers calls are equivalent to:
    //   clang-offload-bundler --list     --type bc --input $FILE
    //   clang-offload-bundler --unbundle --type bc --input $FILE --output $OUT --targets $TARGET
    std::vector<std::string> targets;
    auto bundle = ClangOffloadBundle::parse(hostBCFile);
    if (bundle) {
      if (auto e = bundle->takeError())
        AGV_WARNF("cannot list offload bundles for {}: {}", hostBCFile, toString(std::move(e)));
      else targets = bundle->get().entries ^ map([](auto x) { return x.id; });
    }
    auto extracted = targets ^ collect([&](auto &target) -> std::optional<std::string> {
                       auto targetBCFile = fmt::format("{}.{}-{}.bc", idx, name, target);
                       clang::OffloadBundlerConfig config;
                       config.FilesType = "bc";
                       config.ObjcopyPath = "";
                       config.InputFileNames = {hostBCFile};
                       config.OutputFileNames = {targetBCFile};
                       config.TargetNames = {target};
                       if (auto e = clang::OffloadBundler(config).UnbundleFiles()) {
                         AGV_WARNF("cannot extract target {} from {}", target, hostBCFile);
                         return std::nullopt;
                       }
                       if (verbose)
                         AGV_INFOF("extracted {} from offload bundle {}", targetBCFile, hostBCFile);
                       return {targetBCFile};
                     });
    auto hostBCDest = fmt::format("{}/{}.{}.bc", dest, idx, name);
    if (targets.empty()) saveBC(hostBCFile, hostBCDest); // not an offload bundle, copy the host BC
    else {
      if (extracted.size() != targets.size()) {
        AGV_WARNF(
            "not all BC extracted, got [{}] targets but extracted only [{}], retaining all BCs",
            targets | mk_string(","), extracted | mk_string(","));
        saveBC(hostBCFile, hostBCDest);
      }
      for (auto &file : extracted) { // copy the extracted targets then delete
        saveBC(file, fmt::format("{}/{}", dest, file));
        if (auto e = sys::fs::remove(file, true))
          AGV_WARNF("cannot remove extracted temporary {}", file);
      }
    }
  }

  return codes;
}

struct Task {
  size_t idx;
  CompileCommand cmd;
  std::string pchFile;
  std::error_code error;
  std::shared_ptr<raw_ostream> stream;
  struct Result {
    CompileCommand cmd;
    std::optional<std::string> pchName;
    std::vector<agv::ClangDatabase::Bitcode> bitcodes;
    std::map<std::string, std::string> dependencies;
    std::vector<std::string> diagnostics;
  };
};

static Task::Result runCompileJobs(bool verbose, Task &task, const std::string &wd,
                                   const std::string &dest,
                                   const std::unordered_map<std::string, std::string> &programLUT) {
  auto program = task.cmd.CommandLine[0];
  if (!sys::path::is_absolute(program)) {
    if (auto it = programLUT.find(program); it != programLUT.end()) { program = it->second; }
  }
  auto programName = sys::path::filename(program).str();

  auto name = sys::path::stem(task.cmd.Filename).str();
  auto iiFile = wd + "/" + name + ".ii";
  auto pchFile = wd + "/" + name + ".pch";
  auto dFile = wd + "/" + name + ".d";

  auto isOMP = task.cmd.CommandLine ^ exists([](auto x) { return x ^ starts_with("-fopenmp"); });
  auto noOffloadArch = [&](auto &arg) { return !isOMP || !(arg ^ starts_with("--offload-arch")); };

  auto args = task.cmd.CommandLine                                                               //
              | zip_with_index()                                                                 //
              | bind([](auto &s, auto idx) {                                                     //
                  if (s == "-o") return std::vector<size_t>{idx, idx + 1};                       //
                  else if (s ^ starts_with("-o")) return std::vector<size_t>{idx};               //
                  return std::vector<size_t>{};                                                  //
                })                                                                               //
              | and_then([&](auto x) {                                                           //
                  std::unordered_set<size_t> discardIndices(x.begin(), x.end());                 //
                  return task.cmd.CommandLine                                                    //
                         | zip_with_index()                                                      //
                         | filter([&](auto, auto idx) { return !discardIndices.contains(idx); }) //
                         | keys()                                                                //
                         | tail()                                                                //
                         | to_vector();
                });

  auto bcArgs = std::vector{program, "-emit-llvm"s} | concat(args) | to_vector();
  auto pchArgs =                                                                           //
      std::vector{program, "-emit-ast"s, "-o" + pchFile, "--offload-host-only"s, "-MD"s} | //
      concat(args | filter(noOffloadArch)) | to_vector();

  //  auto iiArgs = std::vector{program, "-E"s, "-o"s + iiFile, "--offload-host-only"s} //
  //                | concat(args) | to_vector();

  std::vector driverTaskArgs{bcArgs, pchArgs};
  std::vector<std::string> driverDiags(driverTaskArgs.size());
  agv::par_for(driverTaskArgs, [&](auto args, auto idx) {
    if (verbose) AGV_COUT << (args ^ mk_string(" ")) << std::endl;
    std::string diag;
    auto code = sys::ExecuteAndWait(program, (args ^ map([](auto &x) -> StringRef { return x; })),
                                    {}, {}, 0, 0, &driverDiags[idx]);
    if (code != 0) AGV_WARNF("non-zero return for `{}`: ", args ^ mk_string(" "), driverDiags[idx]);
  });

  Task::Result result{
      .cmd = task.cmd,
      .pchName = sys::path::filename(task.pchFile).str(),
      .bitcodes = {},
      .dependencies = {},
      .diagnostics = driverDiags,
  };

  { // handle TU.pch CPCH file
    auto buffer = MemoryBuffer::getFile(pchFile);
    if (auto error = buffer.getError()) {
      AGV_WARNF("cannot open PCH {}: {}", pchFile, error.message());
    } else {
      auto ptr = std::move(buffer.get());
      (*task.stream << ptr->getBuffer()).flush();
    }
  }

  { // handle TU.d dependencies
    auto addDep = [&](auto &f) { result.dependencies.emplace(f, f); };
    addDep(task.cmd.Filename);
    std::fstream deps(dFile);
    std::string line;
    while (std::getline(deps, line)) {
      if (line ^ starts_with(pchFile)) continue;
      line                                             //
          ^ filter([](auto &x) { return x != '\\'; })  //
          ^ trim()                                     //
          ^ split(' ')                                 //
          ^ map([](auto &f) { return f ^ trim(); })    //
          ^ filter([](auto &f) { return !f.empty(); }) //
          ^ for_each([&](auto &f) { addDep(f); });     //
    }
  }
  result.bitcodes = collectBitcodeFiles(verbose, task.idx, name, wd, dest);
  return result;
}

static std::vector<Task::Result> runIndexTasks(const std::vector<CompileCommand> &commands,
                                               std::string outDir, bool verbose, bool noCompress) {

  auto [success, failed] =
      (commands | zip_with_index() | map([&](auto &cmd, auto idx) {
         Task task{.idx = idx,
                   .cmd = cmd,
                   .pchFile = fmt::format("{}/{}.{}.pch{}", outDir, idx,
                                          sys::path::filename(cmd.Filename).str(),
                                          (noCompress ? "" : ".zstd")),
                   .error = std::make_error_code(std::errc()),
                   .stream = {}};
         if (noCompress) {
           task.stream = std::make_shared<raw_fd_stream>(task.pchFile, task.error);
         } else {
           task.stream = std::make_shared<agv::utils::zstd_ostream>(task.pchFile, task.error, 6);
         }
         return task;
       }) |
       to_vector()) ^
      partition([](auto &t) { return t.error == std::errc(); });

  std::vector<Task::Result> results(success.size());

  SmallVector<char> currentPath;
  if (auto e = sys::fs::current_path(currentPath); e) {
    AGV_WARNF("cannot get current working directory ({}), compilation cannot proceed", e.message());
    return results;
  }

  // Resolve all non-absolute programs first
  std::unordered_map<std::string, std::string> programLUT;
  for (auto &task : success) {
    auto program = task.cmd.CommandLine[0];
    if (!sys::path::is_absolute(program) && !programLUT.contains(program)) {
      auto name = sys::findProgramByName(program);
      if (auto e = name.getError()) {
        AGV_WARNF("cannot resolve program `{}`: {}", program, e.message());
      } else {
        AGV_INFOF("program {} resolves to {}", program, *name);
        programLUT.emplace(program, *name);
      }
    }
  }
  auto maxFileLength = success ^ fold_left(0, [](auto acc, auto &t) {
                         return std::max(acc, static_cast<int>(t.cmd.Filename.size()));
                       });
  auto logger = agv::ProgressLogger(success.size(), maxFileLength);
  for (auto &[wd, tasks] : success ^ group_by([](auto &task) { return task.cmd.Directory; })) {

    if (auto e = sys::fs::set_current_path(wd); e) {
      AGV_WARNF("cannot change working directory for {} tasks: {}; skipping...", tasks.size(),
                e.message());
      continue;
    }
    AGV_INFOF("cd {}", wd);

    agv::par_for(tasks, [&](Task &task, auto idx) {
      if (task.cmd.CommandLine.empty()) {
        AGV_WARNF("empty command line for file {}", task.cmd.Filename);
        return;
      }
      logger.log(task.cmd.Filename);
      results[task.idx] = runCompileJobs(verbose, task, wd, outDir, programLUT);
    });
  }
  AGV_COUT << std::endl;

  if (auto error = sys::fs::set_current_path(currentPath); error) {
    AGV_CERR << "Cannot restore current working directory (" << error.message()
             << "), terminating..." << std::endl;
    std::exit(1);
  }
  success.clear(); // drop the streams so the file can close

  for (auto &t : failed)
    results.emplace_back(Task::Result{.cmd = t.cmd,
                                      .pchName = {},
                                      .bitcodes = {},
                                      .dependencies = {},
                                      .diagnostics = {t.error.message()}});
  return results;
}

static Expected<agv::index::Options> parseOpts(int argc, const char **argv) {

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

  static cl::opt<bool> clearOutDir( //
      "clear", cl::desc("Clear database output directory even if non-empty."), cl::cat(category));

  static cl::opt<bool> noCompress( //
      "no-compress", cl::desc("Compress individual entries in the database."), cl::cat(category));

  static cl::opt<bool> verbose( //
      "v", cl::desc("Print compile command line used for each translation unit."),
      cl::cat(category));

  static cl::opt<int> maxThreads( //
      "j", cl::desc("Number of jobs in parallel, defaults to total number of threads."),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  if (auto e = agv::parseCategory(category, argc, argv); e) return std::move(*e);

  agv::index::Options options;
  options.buildDir = buildDir.getValue();
  options.sourceGlobs = sourceGlobs;
  options.argsBefore = argsBefore;
  options.argsAfter = argsAfter;

  options.clearOutDir = clearOutDir.getValue();
  options.maxThreads = maxThreads.getValue();
  options.verbose = verbose.getValue();
  options.noCompress = noCompress.getValue();

  if (outDir.empty()) {
    auto lastSegment = sys::path::filename(options.buildDir);
    auto oneBeforeLastSegment = sys::path::filename(sys::path::parent_path(options.buildDir));
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

  std::shared_ptr<CompilationDatabase> db = options.resolveDatabase(adjuster);
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

  SmallVector<char> absOutDirBuffer(options.outDir.begin(), options.outDir.end());
  if (auto err = sys::fs::make_absolute(absOutDirBuffer); err) {
    AGV_ERRF("Cannot resolve absolute path for output dir {}: {}", options.outDir, err.message());
  }
  std::string absOutDir(absOutDirBuffer.begin(), absOutDirBuffer.end());

  auto dbFile = options.outDir + "/db.json";
  std::ofstream dbStream(dbFile, std::ios::trunc);
  if (!dbStream) {
    AGV_ERRF("Cannot open {} for writing!", dbFile);
    return EXIT_FAILURE;
  }

  auto results = runIndexTasks(commands, absOutDir, options.verbose, options.noCompress);

  std::map<std::string, ClangDatabase::Dependency> dependencies;
  for (auto &result : results) {
    for (auto &[_, file] : result.dependencies) {
      auto buffer = MemoryBuffer::getFile(file, /*isText*/ true);
      if (auto e = buffer.getError()) {
        AGV_WARNF("cannot read dependency {}: {}", file, e.message());
        return e.value();
      }
      sys::fs::file_status status;
      if (auto e = sys::fs::status(file, status); e) {
        AGV_WARNF("cannot stat dependency {}: {}", file, e.message());
        return e.value();
      }
      dependencies.emplace(file,
                           ClangDatabase::Dependency{sys::toTimeT(status.getLastModificationTime()),
                                                     (*buffer)->getBuffer().str()});
    }
  }

  auto dbEntries =
      results | collect([&](auto &result) {
        return result.pchName ^ map([&](auto name) {
                 return std::pair{
                     result.cmd.Filename,
                     ClangDatabase::Entry{.compileCommand = result.cmd.CommandLine ^ mk_string(" "),
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

  nlohmann::json databaseJson = ClangDatabase(                          //
      {{"clangMajorVersion", std::to_string(CLANG_VERSION_MAJOR)},      //
       {"clangMinorVersion", std::to_string(CLANG_VERSION_MINOR)},      //
       {"clangPatchVersion", std::to_string(CLANG_VERSION_PATCHLEVEL)}} //
      ,
      absOutDir, dbEntries, dependencies);

  dbStream << databaseJson;
  return EXIT_SUCCESS;
}

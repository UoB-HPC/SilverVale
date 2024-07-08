#include <iostream>
#include <thread>

#include "clang/Tooling/CommonOptionsParser.h"
// #include "clang/Tooling/CompilationDatabase.h"
// #include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"

#include "p3md/p3md.h"

using namespace aspartame;
using namespace llvm;
using namespace clang::tooling;
using namespace clang;

static std::optional<Error> parseCategory(cl::OptionCategory &category, int &argc,
                                          const char **argv) {
  cl::ResetAllOptionOccurrences();
  cl::HideUnrelatedOptions(category);
  std::string ErrorMessage;
  llvm::raw_string_ostream OS(ErrorMessage);
  if (!cl::ParseCommandLineOptions(argc, argv, "", &OS)) {
    OS.flush();
    return llvm::make_error<llvm::StringError>(ErrorMessage, llvm::inconvertibleErrorCode());
  }
  cl::PrintOptionValues();
  return {};
}

llvm::Expected<p3md::build::Options> p3md::parseBuildOpts(int argc, const char **argv) {

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

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  build::Options options;
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

Expected<p3md::list::Options> p3md::parseListOpts(int argc, const char **argv) {
  static cl::OptionCategory category("List options");

  static cl::opt<std::string> dbPath(
      "db", cl::desc("The path to the P3MD database, as generated using the build command"),
      cl::Required, cl::cat(category));

  static cl::opt<p3md::list::Kind> kind(
      "kind", cl::desc("The kind of data to list"),
      cl::values(
          clEnumValN(p3md::list::Kind::Entry, "entry", "Enable trivial optimizations"),
          clEnumValN(p3md::list::Kind::Dependencies, "deps", "Enable default optimizations")),
      cl::Required, cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);
  return list::Options{dbPath.getValue(), kind};
}

Expected<p3md::run::Options> p3md::parseRunOpts(int argc, const char **argv) {
  static cl::OptionCategory category("Run options");

  static cl::opt<std::string> script(cl::Required, "script",
                                     cl::desc("Path to the Lua script to run."), cl::cat(category));

  static cl::list<std::string> scriptRoot(
      cl::CommaSeparated, "script-roots",
      cl::desc("Additional search paths in CSV format for Lua requires."), cl::cat(category));

  static cl::opt<int> maxThreads(
      "j",
      cl::desc("Global number of jobs to run in parallel, defaults to total number of threads."),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  static cl::list<std::string> args( //
      cl::FormattingFlags::Positional,
      cl::desc(" [... args]\n"
               "Arguments passed to the arg table for the Lua script."),
      cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  return run::Options{.script = script.getValue(),
                      .scriptRoots = scriptRoot | to_vector(),
                      .maxThreads = maxThreads,
                      .args = args | to_vector()};
}

Expected<p3md::def::Options> p3md::parseDefOpts(int argc, const char **argv) {
  static cl::OptionCategory category("Diff options");

  static cl::opt<std::string> output(cl::Optional, "output",
                                     cl::desc("File name of the Teal definition file for scripts; "
                                              "will write to stdout if unspecified."),
                                     cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);
  return def::Options{.output = output.getValue()};
}

template <typename P, typename F>
static int parseAndRun(int argc, const char **argv, P parse, F run) {
  auto maybeOptions = parse(argc, argv);
  if (auto x = maybeOptions.takeError()) {
    std::cerr << toString(std::move(x));
    return EXIT_FAILURE;
  }
  return run(maybeOptions.get());
}

int p3md::build_main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &p3md::parseBuildOpts, &p3md::build::run);
}

int p3md::list_main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &p3md::parseListOpts, &p3md::list::run);
}

int p3md::run_main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &p3md::parseRunOpts, &p3md::run::run);
}

int p3md::def_main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &p3md::parseDefOpts, &p3md::def::run);
}

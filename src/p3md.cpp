#include <iostream>
#include <thread>

#include "tree_sitter/api.h"
#include "tree_sitter_c/api.h"
#include "tree_sitter_cpp/api.h"
#include "tree_sitter_fortran/api.h"

#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"

#include "p3md/p3md.h"

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
      "build", cl::desc("The build directory containing compile_command.json"), //
      cl::cat(category), cl::sub(cl::SubCommand::getAll()));

  static cl::opt<std::string> outDir(
      "out",
      cl::desc("The output directory for storing database files, "
               "defaults to the last 2 segments of the build directory joined with full stop"), //
      cl::init(""), cl::cat(category), cl::sub(cl::SubCommand::getAll()));

  static cl::list<std::string> rootDirs( //
      "root",
      cl::desc("Base path for sources in CSV format, "
               "this also prevents analysis (tree inlining, etc) from escaping this path"),
      cl::ZeroOrMore, cl::CommaSeparated, //
      cl::cat(category), cl::sub(cl::SubCommand::getAll()));

  static cl::list<std::string> argsAfter( //
      "args-before", cl::desc("Extra arguments to prepend to the compiler command line"),
      cl::cat(category), cl::sub(cl::SubCommand::getAll()));

  static cl::list<std::string> argsBefore( //
      "args-after", cl::desc("Extra arguments to append to the compiler command line"),
      cl::cat(category), cl::sub(cl::SubCommand::getAll()));

  static cl::opt<bool> clearOutDir( //
      "clear", cl::desc("Clear database output directory even if non-empty"), cl::cat(category),
      cl::sub(cl::SubCommand::getAll()));

  static cl::opt<int> maxThreads( //
      "j", cl::desc("Number of parallel AST jobs in parallel, defaults to total number of threads"),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category),
      cl::sub(cl::SubCommand::getAll()));

  static cl::opt<std::string> sourceGlob( //
      cl::Positional, cl::desc("Glob pattern for files to include in the database"), cl::init("*"),
      cl::cat(category), cl::sub(cl::SubCommand::getAll()));

  cl::ResetAllOptionOccurrences();
  cl::HideUnrelatedOptions(category);

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  build::Options options;
  options.buildDir = buildDir.getValue();
  options.sourceGlob = sourceGlob.getValue();
  options.argsBefore = argsBefore;
  options.argsAfter = argsAfter;

  options.clearOutDir = clearOutDir.getValue();
  options.maxThreads = maxThreads.getValue();

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

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  return list::Options{dbPath.getValue()};
}

Expected<p3md::diff::Options> p3md::parseDiffOpts(int argc, const char **argv) {
  static cl::OptionCategory category("Diff options");
  static cl::opt<std::string> leftDbPath(
      "l", cl::desc("The path to the first P3MD database, as generated using the build command"),
      cl::Required, cl::cat(category));

  static cl::opt<std::string> rightDbPath(
      "r", cl::desc("The path to the second P3MD database, as generated using the build command"),
      cl::Required, cl::cat(category));

  static cl::list<std::string> entries( //
      cl::Positional, cl::desc("<entry0> [... <entryN>]"), cl::OneOrMore, cl::cat(category),
      cl::sub(cl::SubCommand::getAll()));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  return diff::Options{leftDbPath.getValue(), rightDbPath.getValue(), entries};
}

Expected<p3md::dump::Options> p3md::parseDumpOpts(int argc, const char **argv) {
  static cl::OptionCategory category("Diff options");

  static cl::opt<std::string> dbPath(
      "db", cl::desc("The path to the P3MD database, as generated using the build command"),
      cl::Required, cl::cat(category));

  static cl::list<std::string> Entries( //
      cl::Positional, cl::desc("<entry0> [... <entryN>]"), cl::OneOrMore, cl::cat(category),
      cl::sub(cl::SubCommand::getAll()));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  return dump::Options{dbPath.getValue(), Entries};
}

template <typename P, typename F>
static int parseAndRun(int argc, const char **argv, P parse, F run) {
  auto maybeOptions = parse(argc, argv);
  if (auto x = maybeOptions.takeError()) {
    std::cout << toString(std::move(x));
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

int p3md::dump_main(DataKind kind, int argc, const char **argv) {
  return parseAndRun(argc, argv, &p3md::parseDumpOpts, &p3md::dump::run);
}

int p3md::diff_main(DataKind kind, int argc, const char **argv) {
  return parseAndRun(argc, argv, &p3md::parseDiffOpts, &p3md::diff::run);
}

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
      "build", cl::desc("The build directory containing compile_command.json"), //
      cl::cat(category));

  static cl::opt<std::string> outDir(
      "out",
      cl::desc("The output directory for storing database files, "
               "defaults to the last 2 segments of the build directory joined with full stop"), //
      cl::init(""), cl::cat(category));

  static cl::list<std::string> argsAfter( //
      "args-before", cl::desc("Extra arguments to prepend to the compiler command line"),
      cl::cat(category));

  static cl::list<std::string> argsBefore( //
      "args-after", cl::desc("Extra arguments to append to the compiler command line"),
      cl::cat(category));

  static cl::opt<bool> clearOutDir( //
      "clear", cl::desc("Clear database output directory even if non-empty"), cl::cat(category));

  static cl::opt<int> maxThreads( //
      "j", cl::desc("Number of parallel AST jobs in parallel, defaults to total number of threads"),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  static cl::opt<std::string> sourceGlob( //
      cl::Positional, cl::desc("Glob pattern for files to include in the database"), cl::init("*"),
      cl::cat(category));

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

  static cl::opt<p3md::list::Kind> kind(
      "kind", cl::desc("The kind of data to list"),
      cl::values(
          clEnumValN(p3md::list::Kind::Entry, "entry", "Enable trivial optimizations"),
          clEnumValN(p3md::list::Kind::Dependencies, "deps", "Enable default optimizations")),
      cl::Required, cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);
  return list::Options{dbPath.getValue(), kind};
}

Expected<p3md::diff::Options> p3md::parseDiffOpts(int argc, const char **argv) {
  static cl::OptionCategory category("Diff options");

  static cl::opt<DataKind> kind(
      "kind", cl::desc("Available data for applying diff"), cl::init(DataKind::Source),
      cl::values(
          clEnumValN(DataKind::Source, "source", "Direct source code SLOC diff"),
          clEnumValN(DataKind::TSTree, "tstree", "Tree-sitter tree with comments removed"),
          clEnumValN(DataKind::STree, "stree",
                     "ClangAST based semantic tree with symbols normalised"),
          clEnumValN(DataKind::STreeInline, "stree+i",
                     "ClangAST based semantic tree with symbols normalised and calls inlined")));

  static cl::opt<std::string> root(
      "root",
      cl::desc("The root path shared by all models. "
               "Analysis (diff) will not escape the unions of all root paths."),
      cl::Optional);

  static cl::list<std::string> entries( //
      cl::FormattingFlags::Positional,
      cl::desc("<entry> [... <entryN>]\n"
               "  Compare all databases against the first entry.\n"
               "  Each entry can be suffixed by a colon separated list of root base paths (e.g "
               "db_path_1:rootA:rootB db_path_2:rootC:rootD).\n"
               "  Analysis (diff) will not escape the union of all base paths."),
      cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);
  return diff::Options{kind.getValue(), entries | map([](auto &x) {
                                          auto paths = x ^ split(":");
                                          return std::pair{paths ^ head_maybe() ^ get_or_else(x),
                                                           root.empty()
                                                               ? paths ^ tail()
                                                               : paths ^ tail() ^ append(root)};
                                        }) | to_vector()};
}

Expected<p3md::dump::Options> p3md::parseDumpOpts(int argc, const char **argv) {
  static cl::OptionCategory category("Dump options");

  static cl::opt<std::string> dbPath(
      "db", cl::desc("The path to the P3MD database, as generated using the build command"),
      cl::Required, cl::cat(category));

  static cl::list<std::string> Entries( //
      cl::Positional, cl::desc("<entry0> [... <entryN>]"), cl::OneOrMore, cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  return dump::Options{dbPath.getValue(), Entries};
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

int p3md::dump_main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &p3md::parseDumpOpts, &p3md::dump::run);
}

int p3md::diff_main(int argc, const char **argv) {
  return parseAndRun(argc, argv, &p3md::parseDiffOpts, &p3md::diff::run);
}

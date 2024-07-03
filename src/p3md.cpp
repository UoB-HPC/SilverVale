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

Expected<p3md::diff::Options> p3md::parseDiffOpts(int argc, const char **argv) {
  static cl::OptionCategory category("Diff options");

  static cl::list<std::string> databases( //
      cl::FormattingFlags::Positional,
      cl::desc("<database> [... <databaseN>]\n"
               "  Compare all databases against the base (first by default) database.\n"
               "  Each database can be suffixed by a colon separated list of root base paths (e.g "
               "db_path_1:rootA:rootB db_path_2:rootC:rootD).\n"
               "  Analysis (diff) will not escape the union of all root paths of the database it's "
               "specified on."
               "The base database can be specified via --base <database>"),
      cl::cat(category));

  static cl::opt<std::string> base( //
      "base",
      cl::desc("The base database to compare against, defaults to the "
               "first positional database argument."),
      cl::Optional);

  static cl::list<DataKind> kinds(
      "kinds", cl::CommaSeparated, cl::OneOrMore,
      cl::desc("Comma separated kinds of metric to use for diff operation. Defaults to all "
               "supported kinds."),
      cl::values(
          clEnumValN(DataKind::SLOC, "sloc", "Direct source code diff."),
          clEnumValN(DataKind::LLOC, "lloc", "Direct source code diff."),
          clEnumValN(DataKind::Source, "source", "Direct source code diff."),
          clEnumValN(DataKind::TSTree, "tstree", "Tree-sitter tree with comments removed."),
          clEnumValN(DataKind::STree, "stree",
                     "ClangAST based semantic tree with symbols normalised."),
          clEnumValN(DataKind::STreeInline, "stree+i",
                     "ClangAST based semantic tree with symbols normalised and calls inlined.")));

  static cl::opt<std::string> root(
      "root",
      cl::desc("The root path shared by all databases. "
               "Analysis (diff) will not escape the unions of all root paths."),
      cl::Optional);

  static cl::opt<std::string> outputPrefix(
      "output", cl::desc("The output file name prefix for all result CSV files."), cl::Optional);

  static cl::opt<int> maxThreads( //
      "j",
      cl::desc(
          "Number of parallel AST frontend jobs in parallel, defaults to total number of threads."),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  static cl::opt<std::string> transforms( //
      "transforms", cl::Optional,
      cl::desc( //
          "A semicolon separated sequence of transform steps to apply for all loaded code bases.\n"
          "\n * merge: [merge=<TU glob>:<TU name>] Combine multiple TUs (files) into a single TU "
          "where all TUs matching TU glob will be merged"
          "\n * include: [include=<TU glob>] Include TUs that match the TU glob."
          "\n * exclude: [exclude=<TU glob>] Exclude TUs that match the TU glob."
          "\nFor example, to select all TUs and merge everything into a single TU: `--transforms "
          "\"include=*;merge=*:foo\"`"
          "Note that steps are executed in the order they are specified and steps of the same type "
          "can appear more than once."
          "By default, the include all transform `--transforms \"include=*\"` will be used."),
      cl::cat(category));

  static cl::list<std::string> matches( //
      "matches", cl::ZeroOrMore, cl::CommaSeparated,
      cl::desc(
          "A comma separated glob pairs for matching TUs (files) against the base TUs."
          "The format is <base glob>:<model glob>,... where the diff will run on the first "
          "matching pattern pair; malformed patterns are ignored."
          "This overrides the default behaviour where TUs are matched by identical filenames."),
      cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  // --transform "include=*;exclude=*;merge=A:N;merge=B*:C"
  // --matches "a:b"

  auto pairPattern = [](const std::string &in,
                        char delim) -> std::optional<std::pair<std::string, std::string>> {
    auto pair = in ^ split(delim);
    if (pair.size() == 2) return std::pair{pair[0], pair[1]};
    std::cerr << "Ignoring malformed pair pattern with delimiter (" << delim << "): `" //
              << in << "`" << std::endl;
    return std::nullopt;
  };

  using Step = std::variant<p3md::diff::EntryFilter, p3md::diff::EntryMerge>;

  auto mappedTransforms =
      transforms.empty()
          ? std::vector<Step>{}
          : transforms ^ split(';') ^ collect([&](auto &step) {
              return pairPattern(step, '=') ^
                     bind([&](auto name, auto value) -> std::optional<Step> {
                       if (name != "merge") {
                         return pairPattern(value, ':') ^ map([](auto glob, auto name) -> Step {
                                  return p3md::diff::EntryMerge{.glob = glob, .name = name};
                                });
                       } else if (name != "include")
                         return p3md::diff::EntryFilter{.include = true, .glob = value};
                       else if (name != "exclude")
                         return p3md::diff::EntryFilter{.include = false, .glob = value};
                       else {
                         std::cerr << "Unknown step name: `" << name << "`" << std::endl;
                         return std::nullopt;
                       }
                     });
            });

  auto mappedDbs =
      databases | map([](auto &x) {
        auto paths = x ^ split(":");
        return p3md::diff::Database{paths ^ head_maybe() ^ get_or_else(x),
                                    root.empty() ? paths ^ tail() : paths ^ tail() ^ append(root)};
      }) |
      to_vector();
  return diff::Options{.databases = mappedDbs,
                       .base = base.empty() ? mappedDbs[0].db : base,
                       .kinds = kinds,                                                    //
                       .transforms = mappedTransforms,                                    //
                       .matches = (matches | to_vector()) ^                               //
                                  collect([&](auto &p) { return pairPattern(p, ':'); }) ^ //
                                  map([](auto source, auto target) {                      //
                                    return p3md::diff::EntryMatch{.sourceGlob = source,
                                                                  .targetGlob = target};
                                  }),
                       .outputPrefix = outputPrefix.getValue(),
                       .maxThreads = maxThreads};
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

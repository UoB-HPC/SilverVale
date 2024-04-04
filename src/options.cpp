#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/CompilationDatabase.h"
#include <iostream>

#include "p3md/options.h"

using namespace llvm;
using namespace clang::tooling;
using namespace clang;

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

llvm::Expected<p3md::BuildOptions> p3md::BuildOptions::parse(int &argc, const char **argv) {

  static cl::OptionCategory Category("Build options");

  static cl::opt<std::string> BuildDir(
      "build", cl::desc("The build directory containing compile_command.json"), cl::init("."),
      cl::cat(Category), cl::sub(cl::SubCommand::getAll()));

  static cl::list<std::string> RootDirs(
      "root",
      cl::desc("Base path for sources, this also prevents analysis (tree inlining, etc) from "
               "escaping this path"),
      cl::ZeroOrMore, cl::cat(Category), cl::sub(cl::SubCommand::getAll()));

  static cl::list<std::string> ArgsAfter( //
      "extra-arg", cl::desc("Additional argument to append to the compiler command line"),
      cl::cat(Category), cl::sub(cl::SubCommand::getAll()));

  static cl::list<std::string> ArgsBefore( //
      "extra-arg-before", cl::desc("Additional argument to prepend to the compiler command line"),
      cl::cat(Category), cl::sub(cl::SubCommand::getAll()));

  static cl::list<std::string> SourcePaths( //
      cl::Positional, cl::desc("<source0> [... <sourceN>]"), cl::OneOrMore, cl::cat(Category),
      cl::sub(cl::SubCommand::getAll()));

  cl::ResetAllOptionOccurrences();
  cl::HideUnrelatedOptions(Category);

  BuildOptions options;

  std::string ErrorMessage;
  options.Compilations = FixedCompilationDatabase::loadFromCommandLine(argc, argv, ErrorMessage);
  if (!ErrorMessage.empty()) ErrorMessage.append("\n");
  llvm::raw_string_ostream OS(ErrorMessage);
  // Stop initializing if command-line option parsing failed.
  if (!cl::ParseCommandLineOptions(argc, argv, "", &OS)) {
    OS.flush();
    return llvm::make_error<llvm::StringError>(ErrorMessage, llvm::inconvertibleErrorCode());
  }
  for (int i = 0; i < argc; ++i) {
    std::cout << argv[i] << " " << std::endl;
  }

  cl::PrintOptionValues();

  options.BuildDir = BuildDir.c_str();
  options.Roots = RootDirs;
  options.SourcePathList = SourcePaths;

  if (!options.Compilations) {
    if (auto result = findCompilationDatabaseFromDirectory((getAbsolutePath(BuildDir))); result) {
      options.Compilations = std::move(result->first);
      options.CompilationDBFile = result->second;
    }
    if (!options.Compilations) {
      llvm::errs() << "Error while trying to load a compilation database:\n"
                   << ErrorMessage << "Running without flags.\n";
      options.Compilations =
          std::make_unique<FixedCompilationDatabase>(".", std::vector<std::string>());
    }
  }

  auto AdjustingCompilations =
      std::make_unique<ArgumentsAdjustingCompilations>(std::move(options.Compilations));
  options.Adjuster = getInsertArgumentAdjuster(ArgsBefore, ArgumentInsertPosition::BEGIN);
  options.Adjuster =
      combineAdjusters(std::move(options.Adjuster),
                       getInsertArgumentAdjuster(ArgsAfter, ArgumentInsertPosition::END));
  AdjustingCompilations->appendArgumentsAdjuster(options.Adjuster);
  options.Compilations = std::move(AdjustingCompilations);
  return options;
}

std::optional<std::string> p3md::BuildOptions::resolve(const std::string &sourcePath) const {
  if (llvm::sys::path::is_absolute(sourcePath)) {
    return llvm::sys::fs::is_regular_file(sourcePath) ? std::optional{sourcePath} : std::nullopt;
  }
  for (auto &root : Roots) {
    auto path = root + "/" + sourcePath;
    if (llvm::sys::fs::is_regular_file(path)) return path;
  }
  return {};
}

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

Expected<p3md::ListOptions> p3md::ListOptions::parse(int &argc, const char **argv) {
  static cl::OptionCategory Category("List options");
  static cl::opt<std::string> DbPath(
      "db", cl::desc("The path to the P3MD database, as generated using the build command"),
      cl::Required, cl::cat(Category));

  if (auto e = parseCategory(Category, argc, argv); e) return std::move(*e);

  return ListOptions{DbPath.c_str()};
}

Expected<p3md::DiffOptions> p3md::DiffOptions::parse(int &argc, const char **argv) {
  static cl::OptionCategory Category("Diff options");
  static cl::opt<std::string> LeftDbPath(
      "l", cl::desc("The path to the first P3MD database, as generated using the build command"),
      cl::Required, cl::cat(Category));

  static cl::opt<std::string> RightDbPath(
      "r", cl::desc("The path to the second P3MD database, as generated using the build command"),
      cl::Required, cl::cat(Category));

  static cl::list<std::string> Entries( //
      cl::Positional, cl::desc("<entry0> [... <entryN>]"), cl::OneOrMore, cl::cat(Category),
      cl::sub(cl::SubCommand::getAll()));

  if (auto e = parseCategory(Category, argc, argv); e) return std::move(*e);

  return DiffOptions{LeftDbPath.c_str(), RightDbPath.c_str(), Entries};
}

Expected<p3md::DumpOptions> p3md::DumpOptions::parse(int &argc, const char **argv) {
  static cl::OptionCategory Category("Diff options");

  static cl::opt<std::string> DbPath(
      "db", cl::desc("The path to the P3MD database, as generated using the build command"),
      cl::Required, cl::cat(Category));

  static cl::list<std::string> Entries( //
      cl::Positional, cl::desc("<entry0> [... <entryN>]"), cl::OneOrMore, cl::cat(Category),
      cl::sub(cl::SubCommand::getAll()));

  if (auto e = parseCategory(Category, argc, argv); e) return std::move(*e);

  return DumpOptions{DbPath.c_str(), Entries};
}

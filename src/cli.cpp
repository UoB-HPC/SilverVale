#include <iostream>

#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/CommandLine.h"

#include "agv/cli.h"

using namespace llvm;
using namespace clang::tooling;
using namespace clang;

std::optional<Error> agv::parseCategory(cl::OptionCategory &category, int &argc,
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
agv::ProgressLogger::ProgressLogger(size_t total, int maxLogLength)
    : total(total), maxLogLength(maxLogLength) {}
void agv::ProgressLogger::log(const std::string &line, bool progress) {
  auto &&s = AGV_COUT << "# [" << (progress ? completed++ : completed.load()) << "/" << total
                      << "] " << std::left << std::setw(maxLogLength + 10) << line;
  if (progress) (s << "\r").flush();
  else s << std::endl;
}

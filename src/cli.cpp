#include "sv/cli.h"

#include "llvm/Support/CommandLine.h"

using namespace llvm;
using namespace clang::tooling;
using namespace clang;

std::optional<Error> sv::parseCategory(cl::OptionCategory &category, int &argc, const char **argv) {
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
sv::ProgressLogger::ProgressLogger(size_t total, int maxLogLength)
    : total(total), maxLogLength(maxLogLength) {}
void sv::ProgressLogger::log(const std::string &line, bool progress) {
  auto s = SV_COUT << "# [" << (progress ? completed++ : completed.load()) << "/" << total
                      << "] " << std::left << std::setw(maxLogLength + 10) << line;
  if (progress) (s << "\r").flush();
  else s << std::endl;
}

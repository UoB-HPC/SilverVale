#include "sv/cli.h"

sv::ProgressLogger::ProgressLogger(size_t total, int maxLogLength)
    : total(total), maxLogLength(maxLogLength) {}
void sv::ProgressLogger::log(const std::string &line, bool progress) {
  auto s = SV_COUT << "# [" << (progress ? completed++ : completed.load()) << "/" << total
                      << "] " << std::left << std::setw(maxLogLength + 10) << line;
  if (progress) (s << "\r").flush();
  else s << std::endl;
}

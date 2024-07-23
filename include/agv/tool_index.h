#pragma once

#include "database.h"
#include <string>
#include <vector>

namespace agv::index {

struct Options {
  std::filesystem::path buildDir;
  std::vector<std::string> sourceGlobs;
  std::filesystem::path outDir;
  bool clearOutDir;
  bool verbose;
  int maxThreads;

  [[nodiscard]] std::unique_ptr<agv::CompilationDatabase> resolveDatabase() const;
};

[[nodiscard]] int main(int argc, const char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace agv::index
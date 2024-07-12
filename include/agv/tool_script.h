#pragma once

#include <string>
#include <vector>

namespace agv::script {

struct Options {
  std::vector<std::string> roots;
  bool defs;
  bool noBuffer;
  int maxThreads;
  std::vector<std::string> args;
};

[[nodiscard]] int main(int argc, const char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace agv::script
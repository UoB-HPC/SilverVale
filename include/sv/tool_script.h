#pragma once

#include <string>
#include <vector>

namespace sv::script {

constexpr const char *Name = "script";
constexpr const char *Description = "Execute Lua scripts with SV objects in scope";

struct Options {
  std::vector<std::string> roots;
  bool defs;
  bool noBuffer;
  int maxThreads;
  std::vector<std::string> args;
};

[[nodiscard]] int main(int argc, char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace sv::script
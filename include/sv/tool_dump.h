#pragma once

#include <string>

namespace sv::dump {

struct Options {
  std::filesystem::path dbDir;
  std::vector<std::string> roots;
};

constexpr const char *Name = "dump";
constexpr const char *Description = "Dump entries in an SV database";

[[nodiscard]] int main(int argc, char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace sv::dump
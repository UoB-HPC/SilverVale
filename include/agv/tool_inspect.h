#pragma once

#include <string>

namespace agv::inspect {

enum class Kind { Entry, Dependencies };
struct Options {
  std::string dbDir;
  Kind kind;
};

[[nodiscard]] int main(int argc, const char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace agv::inspect
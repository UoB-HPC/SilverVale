#pragma once

#include <string>

namespace sv::dump {

struct Options {
  std::filesystem::path dbDir;
};

[[nodiscard]] int main(int argc, const char **argv);
[[nodiscard]] int run(const Options &options);

} // namespace sv::dump
#pragma once

#include "p3md/build.h"
#include "p3md/kind.h"
#include "p3md/list.h"
#include "p3md/run.h"
#include "p3md/def.h"

namespace p3md {

[[nodiscard]] llvm::Expected<build::Options> parseBuildOpts(int argc, const char **argv);
[[nodiscard]] llvm::Expected<list::Options> parseListOpts(int argc, const char **argv);
[[nodiscard]] llvm::Expected<run::Options> parseRunOpts(int argc, const char **argv);
[[nodiscard]] llvm::Expected<def::Options> parseDefOpts(int argc, const char **argv);


[[nodiscard]] int build_main(int argc, const char **argv);
[[nodiscard]] int list_main(int argc, const char **argv);
[[nodiscard]] int run_main(int argc, const char **argv);
[[nodiscard]] int def_main(int argc, const char **argv);

} // namespace p3md
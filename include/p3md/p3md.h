#pragma once

#include "p3md/build.h"
#include "p3md/diff.h"
#include "p3md/dump.h"
#include "p3md/kind.h"
#include "p3md/list.h"

namespace p3md {

[[nodiscard]] llvm::Expected<build::Options> parseBuildOpts(int argc, const char **argv);
[[nodiscard]] llvm::Expected<list::Options> parseListOpts(int argc, const char **argv);
[[nodiscard]] llvm::Expected<diff::Options> parseDiffOpts(int argc, const char **argv);
[[nodiscard]] llvm::Expected<dump::Options> parseDumpOpts(int argc, const char **argv);

[[nodiscard]] int build_main(int argc, const char **argv);
[[nodiscard]] int list_main(int argc, const char **argv);
[[nodiscard]] int diff_main(DataKind kind, int argc, const char **argv);
[[nodiscard]] int dump_main(DataKind kind, int argc, const char **argv);

} // namespace p3md
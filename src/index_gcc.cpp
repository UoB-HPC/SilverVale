#include <filesystem>
#include <fstream>
#include <iostream>

#include "sv/cli.h"
#include "sv/compress.h"
#include "sv/exec.h"
#include "sv/index_common.h"
#include "sv/index_gcc.h"
#include "sv/par.h"

#include "fmt/core.h"
#include "xxh3.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;

bool sv::detectGccAndIndex(bool verbose,
                           const sv::CompilationDatabase::Entry &cmd, //
                           const std::filesystem::path &wd,           //
                           const std::filesystem::path &dest,         //
                           const std::unordered_map<std::string, std::string> &programLUT) {

  auto programAndVersion = sv::resolveProgramAndDetect(
      cmd.command[0],
      [](auto &x) {
        return x ^ starts_with("gcc (GCC)") || //
               x ^ starts_with("g++ (GCC)") || //
               x ^ starts_with("GNU Fortran");
      },
      programLUT);
  if (!programAndVersion) return false;

  const auto &[program, version] = *programAndVersion;
  const auto prefix = fmt::format("{:08x}", XXH32(cmd.file.data(), cmd.file.size(), {}));
  const auto name = std::filesystem::path(cmd.file).stem();
  const auto iiName = fmt::format("{}.ii", name);
  const auto dFile = fmt::format("{}.d", name);

  const auto namedSTreeFile = fmt::format("{}.{}.named.stree.json", prefix, name);
  const auto unnamedSTreeFile = fmt::format("{}.{}.unnamed.stree.json", prefix, name);
  const auto namedIRTreeFile = fmt::format("{}.{}.named.irtree.json", prefix, name);
  const auto unnamedIRTreeFile = fmt::format("{}.{}.unnamed.irtree.json", prefix, name);

  auto envs = {//
               std::pair{"UPROOT_NAMED_STREE_PATH", dest / namedSTreeFile},
               std::pair{"UPROOT_UNNAMED_STREE_PATH", dest / unnamedSTreeFile},
               std::pair{"UPROOT_NAMED_IRTREE_PATH", dest / namedIRTreeFile},
               std::pair{"UPROOT_UNNAMED_IRTREE_PATH", dest / unnamedIRTreeFile},
  // TODO don't hard code LD_PRELOAD
#if !defined(NDEBUG) //&& !defined(__GNUG__)
               std::pair{"LD_PRELOAD",
                         std::filesystem::path(
                             "/usr/lib/clang/17/lib/x86_64-redhat-linux-gnu/libclang_rt.asan.so")}
#endif
  };
  auto envLine =
      envs | mk_string("env ", " ", "", [](auto k, auto v) { return fmt::format("{}={}", k, v); });

  auto execParent = std::filesystem::canonical("/proc/self/exe").parent_path();

  const auto treeArgs = //
      std::vector<std::string>{program,
                               fmt::format("-fplugin={}", execParent / "libuproot_gcc.so")} |
      concat(cmd.command | drop(1)) | to_vector();
  const auto iiArgs = //
      std::vector<std::string>{program, "-E", "-o" + iiName, "-MD"} |
      concat(sv::stripHeadAndOArgs(cmd.command)) | to_vector();

  sv::par_for(std::vector{treeArgs, iiArgs}, [&](auto args, auto) {
    auto line = args | prepend(envLine) | mk_string(" ");
    if (verbose) SV_COUT << line << std::endl;
    if (auto code = sv::exec(line, std::cout); code) {
      if (*code != 0) SV_WARNF("non-zero return for `{}`", line);
    } else SV_WARNF("popen failed for `{}`: ", line);
  });

  // For GCC, we only support Fortran and C/C++, which uses a different driver
  std::string language;
  if (auto driver = std::filesystem::path(program).filename(); driver == "gcc") language = "c";
  else if (driver == "g++") language = "cpp";
  else if (driver == "gfortran") language = "fortran";
  else {
    SV_WARNF("cannot determine language from driver ({}) for command: {}", driver,
             cmd.command ^ mk_string(" "));
    language = fmt::format("unknown ({})", driver);
  }

  sv::FlatEntry //
      result{.language = language,
             .file = std::filesystem::path(cmd.file).filename(),
             .command = cmd.command ^ mk_string(" "),
             .preprocessed = readFile(wd / iiName),
             .namedSTreeFile = namedSTreeFile,
             .unnamedSTreeFile = unnamedSTreeFile,
             .namedIRTreeFile = namedIRTreeFile,
             .unnamedIRTreeFile = unnamedIRTreeFile,
             .dependencies = sv::readDepFile(wd / dFile, cmd.file),
             .attributes = {{"version", version}}};

  std::ofstream out(dest / fmt::format("{}.{}.sv.json", prefix, name), std::ios::out);
  out << nlohmann::json(result);
  return true;
}

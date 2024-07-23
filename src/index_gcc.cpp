#include <filesystem>
#include <fstream>
#include <iostream>

#include "agv/cli.h"
#include "agv/compress.h"
#include "agv/exec.h"
#include "agv/index_common.h"
#include "agv/index_gcc.h"
#include "agv/par.h"

#include "fmt/core.h"
#include "xxh3.h"

#include "clang/Tooling/CompilationDatabase.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Program.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace std::string_literals;
using namespace aspartame;
using namespace llvm;

bool agv::detectGccAndIndex(bool verbose,
                            const agv::CompilationDatabase::Entry &cmd, //
                            const std::filesystem::path &wd,            //
                            const std::filesystem::path &dest,          //
                            const std::unordered_map<std::string, std::string> &programLUT) {

  auto programAndVersion = agv::resolveProgramAndDetect(
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
#if !defined(NDEBUG)
               std::pair{"LD_PRELOAD",
                         std::filesystem::path(
                             "/usr/lib/clang/17/lib/x86_64-redhat-linux-gnu/libclang_rt.asan.so")}
#endif
  };

  auto execParent = std::filesystem::canonical("/proc/self/exe").parent_path();

  const auto treeArgs =
      std::vector<std::string>{program,
                               fmt::format("-fplugin={}", execParent / "libuproot_gcc.so")} |
      concat(cmd.command | drop(1)) | to_vector();
  const auto iiArgs = std::vector<std::string>{program, "-E", "-o" + iiName, "-MD"} |
                      concat(agv::stripDashOArgs(cmd.command)) | to_vector();

  for (const auto &[env, v] : envs)
    setenv(env, v.c_str(), true);
  setenv("CCACHE_DISABLE", "1", true);
  agv::par_for(std::vector{treeArgs, iiArgs}, [&](auto args, auto idx) {
    auto line = args ^ mk_string(" ");
    if (verbose) AGV_COUT << line << std::endl;
    if (auto code = agv::exec(line, std::cout); code) {
      if (*code != 0) AGV_WARNF("non-zero return for `{}`", line);
    } else AGV_WARNF("popen failed for `{}`: ", line);
  });
  for (const auto &[env, _] : envs)
    unsetenv(env);

  agv::FlatEntry //
      result{.language = "cpp",
             .file = std::filesystem::path(cmd.file).filename(),
             .command = cmd.command ^ mk_string(" "),
             .preprocessed = readFile(wd / iiName),
             .namedSTreeFile = namedSTreeFile,
             .unnamedSTreeFile = unnamedSTreeFile,
             .namedIRTreeFile = namedIRTreeFile,
             .unnamedIRTreeFile = unnamedIRTreeFile,
             .dependencies = agv::readDepFile(wd / dFile, cmd.file),
             .attributes = {{"version", version}}};

  std::ofstream out(dest / fmt::format("{}.{}.sv.json", prefix, name), std::ios::out);
  out << nlohmann::json(result);
  return true;
}

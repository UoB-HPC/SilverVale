#include <filesystem>
#include <iostream>

#include "sv/cli.h"
#include "sv/exec.h"
#include "sv/index_gcc.h"
#include "sv/par.h"
#include "sv/uproot.h"

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

  auto programAndVersion = uproot::resolveProgramAndDetect(
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

  const auto envLine = uproot::createEnvLine(
      uproot::Options{.wd = wd,
                      .dest = dest,
                      .file = std::filesystem::path(cmd.file),
                      .prefix = prefix,
                      .verbose = verbose},
      {
  // TODO don't hard code LD_PRELOAD
#if !defined(NDEBUG) //&& !defined(__GNUG__)
          std::pair{std::string_view("LD_PRELOAD"),
                    std::filesystem::path(
                        "/usr/lib/clang/17/lib/x86_64-redhat-linux-gnu/libclang_rt.asan.so")}
#endif
      });

  const auto execParent = std::filesystem::canonical("/proc/self/exe").parent_path();
  const auto treeArgs = //
      std::vector<std::string>{program,
                               fmt::format("-fplugin={}", execParent / UPROOT_GCC_SO)} |
      concat(cmd.command | drop(1)) | to_vector();
  const auto iiArgs = //
      std::vector<std::string>{program, "-E", "-o" + iiName, "-MD"} |
      concat(uproot::stripHeadAndOArgs(cmd.command)) | to_vector();

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

  auto dependencyFile = dest / fmt::format("{}.{}.{}", prefix, name, EntryDepSuffix);
  writeJSON(dependencyFile, uproot::readDepFile(wd / dFile, cmd.file));

  auto preprocessedFile = dest / fmt::format("{}.{}.ii", prefix, name);
  std::filesystem::copy(wd / iiName, preprocessedFile);

  sv::Database::Entry //
      entry{.language = language,
            .file = std::filesystem::path(cmd.file).filename(),
            .command = cmd.command ^ mk_string(" "),
            .preprocessedFile = preprocessedFile,
            .dependencyFile = dependencyFile,
            .treeFiles =
                {
                    fmt::format("{}.{}.{}", prefix, name, EntryNamedSTreeSuffix),
                    fmt::format("{}.{}.{}", prefix, name, EntryUnnamedSTreeSuffix),
                    fmt::format("{}.{}.{}", prefix, name, EntryNamedIrTreeSuffix),
                    fmt::format("{}.{}.{}", prefix, name, EntryUnnamedIrTreeSuffix),
                },
            .attributes = {{"version", version}}};

  writeJSON(dest / fmt::format("{}.{}.{}", prefix, name, EntrySuffix), entry);
  return true;
}

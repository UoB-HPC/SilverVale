#include <filesystem>
#include <iostream>

#include "sv/cli.h"
#include "sv/exec.h"
#include "sv/index_llvm.h"
#include "sv/par.h"
#include "sv/uproot.h"

#include "fmt/core.h"
#include "xxh3.h"

#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace std::string_literals;
using namespace aspartame;

bool sv::detectClangAndIndex(bool verbose,
                             const sv::CompilationDatabase::Entry &cmd, //
                             const std::filesystem::path &wd,           //
                             const std::filesystem::path &dest,         //
                             const std::unordered_map<std::string, std::string> &programLUT) {

  auto programAndVersion = uproot::resolveProgramAndDetect(
      cmd.command[0],
      [](auto &x) {
        return x ^ starts_with("clang ") || x ^ starts_with("Intel(R) oneAPI DPC++/C++");
      },
      programLUT);
  if (!programAndVersion) return false;

  const auto &[program, version] = *programAndVersion;
  const auto prefix = fmt::format("{:08x}", XXH32(cmd.file.data(), cmd.file.size(), {}));
  const auto stem = std::filesystem::path(cmd.file).stem();
  const auto iiName = fmt::format("{}.ii", stem);
  const auto pchName = fmt::format("{}.pch", stem);
  const auto dFile = fmt::format("{}.d", stem);

  const auto isOMP = cmd.command ^ exists([](auto x) { return x ^ starts_with("-fopenmp"); });
  const auto noOffloadArch = [&](auto &arg) {
    return !isOMP || !(arg ^ starts_with("--offload-arch"));
  };

  const auto coverageFlags = std::unordered_set{"-fprofile-instr-generate"s, "-fcoverage-mapping"s};
  // delete SBCC coverage flags as those alter the IR for coverage accuracy
  const auto args = uproot::stripHeadAndOArgs(cmd.command) ^
                    filter([&](auto &x) { return !coverageFlags.contains(x); });

  const auto bcArgs = std::vector<std::string>{program, "-emit-llvm"} | concat(args) |
                      append("-fno-discard-value-names") | append("-g") | append("-MD") |
                      to_vector(); //

  const auto execParent = std::filesystem::canonical("/proc/self/exe").parent_path();
  const auto iiArgs =                                                      //
      std::vector<std::string>{program,                                    //
                               "-E", "-o" + iiName, "--offload-host-only"} //
      | concat(args) | to_vector();

  sv::par_for(std::vector{bcArgs, iiArgs}, [&](auto args, auto) {
    auto line = args ^ mk_string(" ");
    if (verbose) SV_COUT << line << std::endl;
    if (auto code = sv::exec(line, std::cout); code) {
      if (*code != 0) SV_WARNF("non-zero return for `{}`", line);
    } else SV_WARNF("popen failed for `{}`: ", line);
  });

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

  std::string uprootSO = execParent / UPROOT_CLANG_SO;
  if (auto uprootSOEnv = std::getenv("UPROOT_SO"); uprootSOEnv) {
    uprootSO = execParent / std::string(uprootSOEnv);
  }

  const auto uprootArgs = //
      fmt::format("{} {} -fsyntax-only --offload-host-only -Xclang -load -Xclang {} -Xclang "
                  "-plugin -Xclang uproot {}",
                  envLine, program, uprootSO, args | filter(noOffloadArch) | mk_string(" "));

  if (verbose) SV_COUT << uprootArgs << std::endl;
  if (auto code = sv::exec(uprootArgs, std::cout); code) {
    if (*code != 0) SV_WARNF("non-zero return for `{}`", uprootArgs);
  } else SV_WARNF("popen failed for `{}`: ", uprootArgs);

  // cc1 plugin generates an entry at the expected location with:
  //  - language, if detected
  //  - treeFiles
  //  - attributes
  auto entryFile = dest / fmt::format("{}.{}.{}", prefix, stem, sv::EntrySuffix);
  auto entry = sv::readPacked<sv::Database::Entry>(entryFile);

  std::string language = entry.language;
  if (language.empty()) { // Fallback to driver detection
    if (auto driver = std::filesystem::path(program).filename(); driver == "clang") language = "c";
    else if (driver == "clang++") language = "cpp";
    else {
      SV_WARNF("cannot determine language from cc1 plugin and driver ({}) for command: {}", driver,
               cmd.command ^ mk_string(" "));
      language = fmt::format("unknown ({})", driver);
    }
  }

  auto preprocessedFile = dest / fmt::format("{}.{}.ii", prefix, stem);
  sv::writePacked(preprocessedFile, sv::readFile(wd / iiName));

  auto dependencies = sv::uproot::readDepFile(dFile, cmd.file);
  auto dependencyFile = dest / fmt::format("{}.{}.{}", prefix, stem, sv::EntryDepSuffix);
  sv::writePacked(dependencyFile, dependencies);

  entry.language = language;
  entry.path = std::filesystem::path(cmd.file);
  entry.command = cmd.command ^ mk_string(" ");
  entry.preprocessedFile = preprocessedFile;
  entry.dependencyFile = dependencyFile;
  entry.attributes.emplace("version", version);

  sv::writePacked(entryFile, entry);
  return true;
}

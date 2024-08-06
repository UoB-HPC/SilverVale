#pragma once

#include <chrono>
#include <filesystem>
#include <optional>

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

#include "sv/io.h"
#include "sv/cli.h"
#include "sv/exec.h"

namespace sv::uproot {

constexpr std::string_view UprootWd = "UPROOT_WD";
constexpr std::string_view UprootDest = "UPROOT_DEST";
constexpr std::string_view UprootFile = "UPROOT_FILE";
constexpr std::string_view UprootPrefix = "UPROOT_PREFIX";
constexpr std::string_view UprootVerbose = "UPROOT_VERBOSE";

struct Options {
  std::filesystem::path wd, dest;
  std::string file, prefix;
  bool verbose;
};

inline bool hasEnv(const char *name) {
  if (auto value = std::getenv(name); value) {
    return std::string(value) == "1";
  } else return false;
}

inline std::optional<Options> parseEnv() {
  auto wd = std::getenv(UprootWd.data());
  auto dest = std::getenv(UprootDest.data());
  auto file = std::getenv(UprootFile.data());
  auto prefix = std::getenv(UprootPrefix.data());
  if (wd && dest && file && prefix)
    return Options{.wd = wd,
                   .dest = dest,
                   .file = file,
                   .prefix = prefix,
                   .verbose = hasEnv(UprootVerbose.data())};
  return {};
}

inline std::string
createEnvLine(const Options &options,
              std::initializer_list<std::pair<std::string_view, std::string>> rest = {}) {
  using namespace aspartame;
  return std::vector{
             std::pair{UprootWd, options.wd.string()},
             std::pair{UprootDest, options.dest.string()},
             std::pair{UprootFile, options.file},
             std::pair{UprootPrefix, options.prefix},
             std::pair{UprootVerbose, std::string(options.verbose ? "1" : "0")},
         } |
         concat(rest) |
         mk_string("env ", " ", "", [](auto k, auto v) { return fmt::format("{}={}", k, v); });
}

inline std::optional<std::pair<std::filesystem::path, std::string>>
resolveProgramAndDetect(const std::filesystem::path &program,
                        const std::function<bool(const std::string &)> &predicate,
                        const std::unordered_map<std::string, std::string> &programLUT) {
  using namespace aspartame;
  auto resolved = program;
  if (!program.is_absolute()) {
    if (auto it = programLUT.find(program); it != programLUT.end()) { resolved = it->second; }
  }
  std::stringstream output;
  if (auto code = sv::exec(fmt::format("{} --version", program), output); !code || *code != 0)
    return {};
  return ((output.str() ^ lines())                                                           //
          | collect([&](auto &l) { return predicate(l) ? std::optional{l} : std::nullopt; }) //
          | head_maybe())                                                                    //
         ^ map([&](auto &version) { return std::pair{resolved, version}; });
}


inline std::vector<std::string> stripHeadAndOArgs(const std::vector<std::string> &args) {
  using namespace aspartame;
  return args                                                                               //
         | zip_with_index()                                                                 //
         | bind([](auto &s, auto idx) {                                                     //
             if (s == "-o") return std::vector<size_t>{idx, idx + 1};                       //
             else if (s ^ starts_with("-o")) return std::vector<size_t>{idx};               //
             return std::vector<size_t>{};                                                  //
           })                                                                               //
         | and_then([&](auto x) {                                                           //
             std::unordered_set<size_t> discardIndices(x.begin(), x.end());                 //
             return args                                                                    //
                    | zip_with_index()                                                      //
                    | filter([&](auto, auto idx) { return !discardIndices.contains(idx); }) //
                    | keys()                                                                //
                    | tail()                                                                //
                    | to_vector();
           });
}

inline std::map<std::string, sv::Dependency> readDepFile(const std::filesystem::path &depFile,
                                                         const std::string &source) {
  using namespace aspartame;

  std::map<std::string, sv::Dependency> deps;
  auto addDep = [&](const std::filesystem::path &file) {
    try {
      auto time = std::chrono::system_clock::to_time_t(       //
          std::chrono::clock_cast<std::chrono::system_clock>( //
              std::filesystem::last_write_time(file)));
      deps.emplace(file, sv::Dependency{time, sv::readFile(file)});
    } catch (const std::exception &e) { SV_WARNF("cannot read/stat dependency {}: {}", file, e); }
  };
  addDep(source);

  const auto stem = std::filesystem::path(source).stem();
  std::ifstream stream(depFile);
  std::string line;
  while (std::getline(stream, line)) {
    if (line ^ starts_with(stem)) continue;
    line                                             //
        ^ filter([](auto &x) { return x != '\\'; })  //
        ^ trim()                                     //
        ^ split(' ')                                 //
        ^ map([](auto &f) { return f ^ trim(); })    //
        ^ filter([](auto &f) { return !f.empty(); }) //
        ^ for_each([&](auto &f) { addDep(f); });     //
  }
  return deps;
}

} // namespace sv::uproot
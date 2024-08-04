#include <iostream>

#include "sv/cli.h"
#include "sv/model.h"
#include "sv/tool_dump.h"

#include "aspartame/vector.hpp"
#include "cxxopts.hpp"

using namespace aspartame;

int sv::dump::main(int argc, const char **argv) {
  cxxopts::Options options(Name, Description);
  options.add_options()("db", "The path to the database, as generated using the index command",
                        cxxopts::value<std::string>());
  auto result = options.parse(argc, argv);
  if (result.count("help")) {
    SV_COUT << options.help() << std::endl;
    return EXIT_SUCCESS;
  } else return sv::dump::run(sv::dump::Options{result["db"].as<std::string>()});
}

int sv::dump::run(const Options &options) {
  const Database db = Codebase::loadDB(options.dbDir);
  const Codebase cb = Codebase::load(db, std::cout, true, {}, [&](auto &path) { return true; });

  auto root = std::filesystem::current_path() /
              fmt::format("dump_{}", std::filesystem::path(cb.root).filename());
  std::filesystem::create_directories(root);

  SV_INFOF("Writing dump to {}", root);

  std::atomic_size_t written{};
  auto writeAll = [&](const std::filesystem::path &path, auto f) {
    {
      std::ofstream out(path);
      f(out);
    }
    written++;
    SV_INFOF("Wrote {} ", path);
  };

  writeAll(root / "cov.txt", [&](auto &os) {
    auto xss = (cb.coverage->entries | to_vector())                                            //
               ^ map([](auto &p, auto count) { return std::tuple{p.first, p.second, count}; }) //
               ^ sort();
    for (auto &[name, line, count] : xss)
      os << name << ":" << line << "=" << count << "\n";
  });

  SV_COUT << "entry,path\n";
  for (auto &u : cb.units) {
    SV_COUT << u->name() << "," << u->path() << "\n";

    auto path = std::filesystem::path(u->name());
    auto stem = path.stem(), ext = path.extension();

    writeAll(root / fmt::format("{}.src.raw{}", stem, ext),
             [&](auto &os) { os << u->sourceAsWritten().contentWhitespaceNormalised(); });

    writeAll(root / fmt::format("{}.src.cpp{}", stem, ext),
             [&](auto &os) { os << u->sourcePreprocessed().contentWhitespaceNormalised(); });

    writeAll(root / fmt::format("{}.src.cov{}", stem, ext),
             [&](auto &os) { os << u->sourceWithCoverage().contentWhitespaceNormalised(); });

    writeAll(root / fmt::format("{}.tstree.raw.txt", stem),
             [&](auto &os) { os << u->sourceAsWritten().tsTree().prettyPrint(); });

    writeAll(root / fmt::format("{}.tstree.cpp.txt", stem),
             [&](auto &os) { os << u->sourcePreprocessed().tsTree().prettyPrint(); });

    writeAll(root / fmt::format("{}.tstree.cov.txt", stem),
             [&](auto &os) { os << u->sourceWithCoverage().tsTree().prettyPrint(); });

    for (auto [name, view] : {
             std::pair{"raw", Unit::View::AsIs},
             std::pair{"cov", Unit::View::WithCoverage},
         }) {
      writeAll(root / fmt::format("{}.stree.{}.txt", stem, name),
               [&](auto &os) { os << u->sTree(view).prettyPrint(); });
      writeAll(root / fmt::format("{}.sTreeinlined.{}.txt", stem, name),
               [&](auto &os) { os << u->sTreeInlined(view).prettyPrint(); });
      writeAll(root / fmt::format("{}.irtree.{}.txt", stem, name),
               [&](auto &os) { os << u->irTree(view).prettyPrint(); });
    }
  }

  SV_INFOF("Done writing {} files", written);
  return EXIT_SUCCESS;
}

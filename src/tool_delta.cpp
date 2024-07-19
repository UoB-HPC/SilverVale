#include <fstream>
#include <iostream>
#include <thread>
#include <utility>

#include "agv/cli.h"
#include "agv/diff.h"
#include "agv/glob.h"
#include "agv/model.h"
#include "agv/par.h"
#include "agv/tool_delta.h"

#include "aspartame/optional.hpp"
#include "aspartame/string.hpp"
#include "aspartame/unordered_map.hpp"
#include "aspartame/vector.hpp"
#include "aspartame/view.hpp"

using namespace aspartame;
using namespace llvm;
using namespace agv;

size_t longestCommonPrefixLen(const std::vector<std::string> &strings) {
  if (strings.size() < 2) return 0;
  auto sorted = strings ^ sort();
  auto &first = sorted.front();
  auto &last = sorted.back();
  size_t len = std::min(first.length(), last.length());
  size_t i = 0;
  while (i < len && first[i] == last[i])
    i++;
  return i;
}
using Units = std::vector<std::shared_ptr<Unit>>;
using DeltaFn = std::function<double(const Units &, const Units &)>;
using MaxFn = std::function<std::optional<double>(const Units &)>;

auto treeSelect = [](delta::Kind kind,
                     auto f) -> std::pair<delta::Kind, std::pair<DeltaFn, MaxFn>> {
  return {kind,
          std::pair{
              [&](const Units &lhs, const Units &rhs) -> double {
                return Diff::apted(Tree::combine("root", lhs ^ map([&](auto &x) { return f(x); })),
                                   Tree::combine("root", rhs ^ map([&](auto &x) { return f(x); })));
              },
              [&](const Units &xs) -> std::optional<double> {
                return Tree::combine("root", xs ^ map([&](auto &x) { return f(x); })).nodes();
              },
          }};
};

std::unordered_map<delta::Kind, std::pair<DeltaFn, MaxFn>> fns = {
    {delta::Kind::SLOCRawAbs, std::pair{
                                  [](const Units &, const Units &rhs) -> double {
                                    return rhs ^ fold_left(0, [](auto acc, auto &u) {
                                             return acc + u->source(true).sloc();
                                           });
                                  },
                                  [](const Units &) -> std::optional<double> { return {}; },
                              }},
    {delta::Kind::LLOCRawAbs, std::pair{
                                  [](const Units &, const Units &rhs) -> double {
                                    return rhs ^ fold_left(0, [](auto acc, auto &u) {
                                             return acc + u->source(true).lloc();
                                           });
                                  },
                                  [](const Units &) -> std::optional<double> { return {}; },
                              }},
    {delta::Kind::SourceRawRel,
     std::pair{
         [](const Units &lhs, const Units &rhs) -> double {
           auto value = std::numeric_limits<double>::max();
           auto ls = lhs ^ map([](auto &x) { return x->source(true).content(); });
           do {
             auto lhsSrc =
                 ls ^ fold_left(std::string{}, [](auto &&acc, auto &x) { return acc += x; });
             auto rs = lhs ^ map([](auto &x) { return x->source(true).content(); });
             do {
               auto rhsSrc =
                   rs ^ fold_left(std::string{}, [](auto &&acc, auto &x) { return acc += x; });
               value = std::fmin(value, static_cast<double>(Diff::diff(lhsSrc, rhsSrc)));
             } while (std::next_permutation(rs.begin(), rs.end()));
           } while (std::next_permutation(ls.begin(), ls.end()));
           return value;
         },
         [](const Units &xs) -> std::optional<double> {
           return (xs ^
                   fold_left(std::string{},
                             [](auto &&acc, auto &x) { return acc += x->source(true).content(); }))
               .size();
         },
     }},
    treeSelect(delta::Kind::TSTreeRel, [](auto &u) { return u->source(true).tsTree(); }),
    treeSelect(delta::Kind::TSTreeRawRel, [](auto &u) { return u->source(true).tsTree(); }),

    treeSelect(delta::Kind::STreeRel, [](auto &u) { return u->sTree(); }),
    treeSelect(delta::Kind::STreeInlineRel, [](auto &u) { return u->sTreeInlined(); }),
    treeSelect(delta::Kind::IRTreeRel, [](auto &u) { return u->irTree(); })};

Expected<delta::Options> parseOpts(int argc, const char **argv) {
  using namespace delta;
  static cl::OptionCategory category("Diff options");

  static cl::list<delta::Kind> kinds(
      "kinds", cl::CommaSeparated, cl::OneOrMore,
      cl::desc("Comma separated kinds of metric to use for diff operation. Defaults to all "
               "supported kinds."),
      cl::values(
          clEnumValN(delta::Kind::SLOCRawAbs, "sloc", "Source Lines of Code"),
          clEnumValN(delta::Kind::LLOCRawAbs, "lloc", "Logical Lines of Code"),
          clEnumValN(delta::Kind::SourceRawRel, "source", "Source code (edit difference)"),
          clEnumValN(delta::Kind::TSTreeRawRel, "tstree", "Tree-sitter tree"),

          clEnumValN(delta::Kind::SLOCAbs, "sloc+p", "Source Lines of Code (after preprocessor)"),
          clEnumValN(delta::Kind::LLOCAbs, "lloc+p", "Logical Lines of Code (after preprocessor)"),
          clEnumValN(delta::Kind::SourceRel, "source+p",
                     "Source code (edit difference, after preprocessor)"),
          clEnumValN(delta::Kind::TSTreeRel, "tstree+p", "Tree-sitter tree (after preprocessor)"),

          clEnumValN(delta::Kind::STreeRel, "stree",
                     "AST based semantic tree with symbols normalised."),
          clEnumValN(delta::Kind::STreeInlineRel, "stree+i",
                     "ClangAST based semantic tree with symbols normalised and calls inlined."),
          clEnumValN(delta::Kind::IRTreeRel, "irtree",
                     "ClangAST based semantic tree with symbols normalised and calls inlined.")));

  static cl::opt<std::string> root(
      "root",
      cl::desc("Root path shared by all databases."
               "Analysis (delta) will not escape the unions of all root paths."),
      cl::Optional);

  static cl::opt<std::string> outputPrefix(
      "output", cl::desc("The output file name prefix for all result CSV files."), cl::Optional);

  static cl::opt<int> maxThreads( //
      "j",
      cl::desc(
          "Number of parallel AST frontend jobs in parallel, defaults to total number of threads."),
      cl::init(std::thread::hardware_concurrency()), cl::cat(category));

  static cl::list<std::string> excludes( //
      "excludes", cl::OneOrMore, cl::PositionalEatsArgs,
      cl::desc("<TU glob> Exclude TUs that match the TU glob."), cl::cat(category));

  static cl::list<std::string> merges( //
      "merges", cl::OneOrMore, cl::PositionalEatsArgs,
      cl::desc("<TU glob>:<TU name> Combine multiple TUs (files) into a single TU where all TUs "
               "matching TU glob will be merged"),
      cl::cat(category));

  static cl::list<std::string> matches( //
      "matches", cl::ZeroOrMore, cl::PositionalEatsArgs,
      cl::desc(
          "A comma separated glob pairs for matching TUs (files) against the base TUs."
          "The format is <base glob>:<model glob>,... where the diff will run on the first "
          "matching pattern pair; malformed patterns are ignored."
          "This overrides the default behaviour where TUs are matched by identical filenames."),
      cl::cat(category));

  static cl::opt<std::string> base( //
      "base",
      cl::desc("The base database to compare against, defaults to the first positional database "
               "argument."),
      cl::Optional);

  static cl::list<std::string> databases( //
      cl::FormattingFlags::Positional,
      cl::desc("<database> [... <database N>]\n"
               "  Compare all databases against the base database.\n"
               "  Each database can optionally be suffixed by a colon separated list of root paths "
               "(e.g db_path_1:rootA:rootB).\n"
               "  When applicable, tree analysis will not escape the union of all root paths."
               "The base database can be specified via --base <database>"),
      cl::cat(category));

  if (auto e = parseCategory(category, argc, argv); e) return std::move(*e);

  auto pairPattern = [](const std::string &in,
                        char delim) -> std::optional<std::pair<std::string, std::string>> {
    auto pair = in ^ split(delim);
    if (pair.size() == 2) return std::pair{pair[0], pair[1]};
    std::cerr << "Ignoring malformed pair pattern with delimiter (" << delim << "): `" //
              << in << "`" << std::endl;
    return std::nullopt;
  };

  auto mappedDbs =
      databases | map([](auto &x) {
        auto paths = x ^ split(":");
        return DatabaseSpec{paths ^ head_maybe() ^ get_or_else(x),
                            root.empty() ? paths ^ tail() : paths ^ tail() ^ append(root)};
      }) |
      to_vector();

  return Options{.databases = mappedDbs,
                 .base = base.empty()
                             ? mappedDbs ^ head_maybe() ^
                                   fold([](auto &d) { return d.path; }, []() { return ""; })
                             : base,
                 .kinds = kinds, //
                 .excludes = excludes | map([](auto &s) { return ExcludeFilter{s}; }) | to_vector(),
                 .merges = merges | collect([&](auto &s) {
                             return pairPattern(s, ':') ^ map([&](auto &glob, auto &name) {
                                      return EntryMerge{.glob = glob, .name = name};
                                    });
                           }) |
                           to_vector(),
                 .matches = matches | collect([&](auto &p) { return pairPattern(p, ':'); }) |
                            map([](auto &source, auto &target) { //
                              return EntryMatch{.sourceGlob = source, .targetGlob = target};
                            }) |
                            to_vector(),
                 .outputPrefix = outputPrefix.getValue(),
                 .maxThreads = maxThreads};
}

int delta::main(int argc, const char **argv) { return parseAndRun(argc, argv, &parseOpts, &run); }

int delta::run(const delta::Options &options) {

  tbb::global_control global_limit(tbb::global_control::max_allowed_parallelism,
                                   options.maxThreads);

  if (options.databases.empty()) {
    std::cerr << "At least 1 database required for comparison" << std::endl;
    return EXIT_FAILURE;
  }

  // P3MD_COUT << "# Using base glob pattern: " << (options.baseGlobs ^ mk_string(", ")) <<
  // std::endl; P3MD_COUT << "# Using pair glob pattern: "
  //           << (options.entryGlobPairs.empty()
  //                   ? "(filename match)"
  //                   : (options.entryGlobPairs ^
  //                      mk_string(", ", [](auto &l, auto &r) { return l + " -> " + r; })))
  //           << std::endl;

  AGV_COUT << "# Loading " << options.databases.size() << " models ..." << std::endl;

  struct Model {
    std::string path;
    std::vector<std::pair<std::string, Units>> entries;
  };

  std::vector<Model> models(options.databases.size());
  par_for(options.databases, [&](auto &spec, auto idx) {
    auto dbFile = spec.path + "/db.json";
    const auto db = Database::fromJsonFile(dbFile);
    const auto excludes = options.excludes ^ map([](auto &f) { return globToRegex(f.glob); });
    const auto cb = Codebase::load(db, std::cout, true, spec.path, {}, [&](auto &path) {
      return excludes ^ forall([&](auto &r) { return !std::regex_match(path, r); });
    });
    const auto merges =
        options.merges ^ map([](auto &m) { return std::pair{globToRegex(m.glob), m.name}; });
    models[idx].path = cb.path;
    models[idx].entries =                                                                     //
        cb.units                                                                              //
        ^ group_by([&](auto &u) {                                                             //
            return (merges                                                                    //
                    | filter([&](auto &r, auto &) { return std::regex_match(u->path(), r); }) //
                    | values()                                                                //
                    | head_maybe()) ^                                                         //
                   get_or_else(u->name());                                                    //
          })                                                                                  //
        ^ to_vector() ^ sort_by([](auto &name, auto &us) {                                    //
            return std::pair{
                name, //
                us ^ fold_left(0, [](auto acc, auto &u) { return acc + u->source(true).sloc(); })};
          });
    std::cout << "# [ " << models[idx].path << " ]" << std::endl;
    for (auto &[name, us] : models[idx].entries) {
      std::cout << "# " << name << " -> {"
                << (us ^ mk_string(", ", [](auto &u) { return u->path(); })) << "\n";
    }
  });
  AGV_COUT << "# All models loaded" << std::endl;

  if (auto ls = models ^ head_maybe(); ls) {
    std::vector<std::tuple<delta::Kind, DeltaFn, std::string, size_t, Units, Units>> tasks;

    fns | filter([&](auto &k, auto) { return options.kinds ^ contains(k); })            //
        | for_each([&](auto &k, auto &p) {                                              //
            ls->entries | for_each([&](auto &lhsName, const Units &l) {                 //
              models | zip_with_index() | for_each([&](auto &r, size_t idx) {           //
                r.entries                                                               //
                    | filter([&](auto &rhsName, auto &) { return lhsName == rhsName; }) //
                    | for_each([&](auto &, const Units &r) {                            //
                        tasks.emplace_back(k, p.first, lhsName, idx, l, r);
                      });
              });
            });
          });

    std::cout << tasks.size() << "\n";

    std::vector<std::tuple<delta::Kind, std::string, size_t, double>> deltas(tasks.size());
    auto taskReverseSizes = tasks ^ sort_by([](auto, auto, auto, auto, auto &l, auto &r) {
                              return -(l | concat(r) | fold_left(0, [](auto acc, auto &u) {
                                         return acc                                 //
                                                + u->sTree().nodes()                //
                                                + u->sTreeInlined().nodes()         //
                                                + u->irTree().nodes()               //
                                                + u->source(true).tsTree().nodes(); //
                                       }));
                            });

    {
      auto logger = ProgressLogger{
          tasks.size(), tasks ^ fold_left(int{}, [](auto acc, auto &e) {
                          return std::max(acc, static_cast<int>(std::get<std::string>(e).size()));
                        })};
      par_for(taskReverseSizes, [&](auto &task, size_t idx) {
        auto &[k, f, name, modelIdx, l, r] = task;
        logger.log(name);
        deltas[idx] = {k, name, modelIdx, f(l, r)};
      });
    }

    size_t prefixLen = longestCommonPrefixLen(models ^ map([&](auto &m) { return m.path; }));
    auto m =
        (deltas ^ group_map([](auto, auto, auto &modelIdx, auto) { return modelIdx; }, //
                            [](auto &kind, auto &name, auto &, auto &value) {
                              return std::tuple{kind, name, value};
                            }))                                  //
        ^ to_vector()                                            //
        ^ sort_by([](auto &modelIdx, auto) { return modelIdx; }) //
        ^ bind([&](auto &modelIdx, auto &xs) {
            auto modelName = models[modelIdx].path.substr(prefixLen);
            return xs                                                                            //
                   ^ sort_by([](auto &kind, auto &name, auto) { return std::pair{kind, name}; }) //
                   ^ map([&](auto &kind, auto &name, auto &value) {                              //
                       return modelIdx == 0 ? std::vector{std::string(to_string(kind)), name,    //
                                                          std::to_string(value)}                 //
                                            : std::vector{std::to_string(value)};                //
                     })                                                                          //
                   ^ prepend(modelIdx == 0                                                       //
                                 ? std::vector<std::string>{"kind", "name", modelName}           //
                                 : std::vector{modelName})                                       //
                   ^ transpose();
          }) //
        ^ transpose();

    for (auto row : m) {
      AGV_COUT << (row ^ mk_string(", ")) << std::endl;
    }

    //    m.x;

    //              model
    // kind, entry
  }

  //  auto outputPrefix = options.outputPrefix.empty() ? "" : options.outputPrefix + ".";
  //
  //  auto lhsEntriesSorted =
  //      (models ^ find([&](auto &m) { return m.dir == options.base; }) ^
  //       fold(
  //           [](auto &m) {
  //             P3MD_COUT << "# Using base model " << m.dir << std::endl;
  //             return m;
  //           },
  //           [&]() {
  //             P3MD_COUT << "# Base model " << options.base
  //                       << " not found, using the first database: " << models[0].dir <<
  //                       std::endl;
  //             return models[0];
  //           }))
  //          .entries;
  //
  //  if (models.size() == 1) {
  //    auto commonPrefixLen = lhsEntriesSorted                      //
  //                           ^ map([](auto &e) { return e.file; }) //
  //                           ^ and_then(&longestCommonPrefixLen);  //
  //
  //    auto model =
  //        DiffModel{lhsEntriesSorted //
  //                      | map([&](auto &e) { return e.file.substr(commonPrefixLen); }) |
  //                      to_vector(),
  //                  options.kinds, lhsEntriesSorted.size()};
  //
  //    auto logger =
  //        ProgressLogger{lhsEntriesSorted.size() * lhsEntriesSorted.size() * options.kinds.size(),
  //                       lhsEntriesSorted | fold_left(int{}, [](auto acc, auto &e) {
  //                         return std::max(acc, static_cast<int>(e.file.size()));
  //                       })};
  //
  //    P3MD_COUT << "# Single model mode: comparing model against itself with "
  //              << (lhsEntriesSorted.size() * lhsEntriesSorted.size()) << " entries total."
  //              << std::endl;
  //    par_for(lhsEntriesSorted, [&](auto &lhs, auto lhsIdx) {
  //      par_for(options.kinds, [&](auto kind, auto) {
  //        model.set(lhsIdx, kind, lhs.file.substr(commonPrefixLen));
  //        par_for(lhsEntriesSorted, [&, state = DiffState{kind, lhs}](auto &rhs, auto rhsIdx) {
  //          model.set(lhsIdx, rhsIdx, kind, rhs.fileName(), state.diff(DiffState{kind, rhs}));
  //          logger.log(rhs.file);
  //        });
  //        logger.log(lhs.file, false);
  //      });
  //    });
  //    P3MD_COUT << std::endl;
  //    model.dump(outputPrefix);
  //  } else {
  //    // XXX insert a null model after the reference model
  //    models.insert(std::next(models.begin()), Model::makeEmpty());
  //
  //    auto commonPrefixLen = (models                                      //
  //                            | map([](auto &m) { return m.dir; })        //
  //                            | filter([](auto x) { return !x.empty(); }) //
  //                            | to_vector()) ^
  //                           and_then(&longestCommonPrefixLen);
  //
  //    auto model = DiffModel{models ^ map([&](auto &m) {
  //                             return m.dir.empty() ? "(max)" : m.dir.substr(commonPrefixLen);
  //                           }),
  //                           options.kinds, lhsEntriesSorted.size()};
  //
  //    auto globPairs = options.matches ^ map([](auto &m) {
  //                       return std::pair{globToRegex(m.sourceGlob), globToRegex(m.targetGlob)};
  //                     });
  //    auto logger = ProgressLogger{lhsEntriesSorted.size() * models.size() * options.kinds.size(),
  //                                 lhsEntriesSorted | fold_left(int{}, [](auto acc, auto &e) {
  //                                   return std::max(acc, static_cast<int>(e.file.size()));
  //                                 })};
  //    par_for(lhsEntriesSorted, [&](auto &lhs, auto lhsIdx) {
  //      auto lhsFileName = lhs.fileName();
  //      auto diffGlob = globPairs ^ find([&](auto &baseGlob, auto &) {
  //                        return std::regex_match(lhsFileName, baseGlob);
  //                      });
  //      par_for(options.kinds, [&](auto kind, auto) {
  //        model.set(lhsIdx, kind, lhsFileName);
  //        par_for(models, [&, state = DiffState(kind, lhs)](auto &rhsRef, auto rhsIdx) {
  //          if (rhsRef.dir.empty()) {         // null model, set upper bounds
  //            model.set(lhsIdx, rhsIdx, kind, //
  //                      lhsFileName, state.max());
  //          } else {
  //            auto rhs = //
  //                rhsRef.entries ^ find([&](auto &rhs) {
  //                  return diffGlob ^
  //                         fold([&](auto &,
  //                                  auto &glob) { return std::regex_match(rhs.fileName(), glob);
  //                                  },
  //                              [&]() { return rhs.fileName() == lhsFileName; });
  //                });
  //            model.set(lhsIdx, rhsIdx, kind, //
  //                      rhs ? rhs->fileName() : "?",
  //                      rhs ? state.diff(DiffState{kind, *rhs})
  //                          : std::numeric_limits<double>::infinity());
  //          }
  //          logger.log(lhsFileName);
  //        });
  //      });
  //    });
  //    P3MD_COUT << std::endl;
  //    model.dump(outputPrefix);
  //  }

  return EXIT_SUCCESS;
}

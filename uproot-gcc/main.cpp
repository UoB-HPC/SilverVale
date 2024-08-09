#include "tree_gimple.h"

#include "context.h"
#include "plugin-version.h"

#include "sv/database.h"
#include "sv/uproot.h"

[[maybe_unused]] int plugin_is_GPL_compatible{};
[[maybe_unused]] int plugin_init(plugin_name_args *args, plugin_gcc_version *version) {
  SV_INFOF("[uproot] Plugin loaded (name={})", args->base_name);
  if (!plugin_default_version_check(version, &gcc_version)) {
    std::cerr << "[uproot] was compiled for  " << GCCPLUGIN_VERSION_MAJOR << "."
              << GCCPLUGIN_VERSION_MINOR << " but got " << version->basever << std::endl;
    return 1;
  }

  static plugin_info pluginInfo = {
      "0.0.1",
      "This plugins collects GIMPLE and prepares a JSON database for further processing ."};
  register_callback(args->base_name, PLUGIN_INFO, nullptr, &pluginInfo);
  register_callback(
      args->base_name, PLUGIN_START_UNIT,
      [](void *, void *data) {
        const auto name = reinterpret_cast<const char *>(data);
        auto basename = main_input_basename;
        SV_INFOF("[uproot] Starting to uproot unit {}", basename);
        auto options = sv::uproot::parseEnv();

        for (auto [afterPass, namedTreeFile, unnamedTreeFile] : {
                 std::tuple{
                     "*warn_unused_result",      //
                     sv::EntryNamedSTreeSuffix,  //
                     sv::EntryUnnamedSTreeSuffix //
                 },
                 std::tuple{
                     "optimized",                 //
                     sv::EntryNamedIrTreeSuffix,  //
                     sv::EntryUnnamedIrTreeSuffix //
                 } //
             }) {

          const auto stem = std::filesystem::path(options->file).stem();
          const auto pass = new sv::GimpleUprootPass(
              g, basename, afterPass,
              options->dest / fmt::format("{}.{}.{}", options->prefix, stem, namedTreeFile),
              options->dest / fmt::format("{}.{}.{}", options->prefix, stem, unnamedTreeFile));

          register_pass_info info{
              .pass = pass,
              .reference_pass_name = afterPass,
              .ref_pass_instance_number = 1,
              .pos_op = PASS_POS_INSERT_AFTER,
          };
          const auto flush = [](void *, void *data) {
            (reinterpret_cast<sv::GimpleUprootPass *>(data))->flush();
          };
          register_callback(name, PLUGIN_PASS_MANAGER_SETUP, nullptr, &info);
          register_callback(name, PLUGIN_FINISH_UNIT, flush, pass);
        }
      },
      args->base_name);
  return 0;
}

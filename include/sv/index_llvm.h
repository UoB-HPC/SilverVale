#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <unordered_map>

#include "sv/model.h"

namespace sv {
bool detectClangAndIndex(bool verbose,
                         const sv::CompilationDatabase::Entry &cmd, //
                         const std::filesystem::path &wd,           //
                         const std::filesystem::path &dest,         //
                         const std::unordered_map<std::string, std::string> &programLUT);
}
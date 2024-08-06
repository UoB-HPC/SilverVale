#pragma once

#include <filesystem>
#include <optional>
#include <string>
#include <unordered_map>

#include "sv/database.h"

namespace sv {

[[nodiscard]] std::optional<std::pair<std::filesystem::path, std::string>>
resolveProgramAndDetect(const std::filesystem::path &program,
                        const std::function<bool(const std::string &)> &predicate,
                        const std::unordered_map<std::string, std::string> &programLUT);

[[nodiscard]] std::vector<std::string> stripHeadAndOArgs(const std::vector<std::string> &args);

[[nodiscard]] std::map<std::string, sv::Dependency>
readDepFile(const std::filesystem::path &depFile, const std::string &source);

} // namespace sv
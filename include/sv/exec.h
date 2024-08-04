#pragma once

#include <array>
#include <filesystem>
#include <optional>
#include <string>

namespace sv {

inline std::optional<std::filesystem::path> findProgramByName(const std::string &name) {
  auto env = std::getenv("PATH");
  if (!env) return {};
  std::stringstream paths(env);
  std::string path;
  while (std::getline(paths, path, ':')) {
    auto program = std::filesystem::path(path) / name;
    std::error_code error;
    if (auto status = std::filesystem::status(program, error); error) {
      auto perms = status.permissions();
      if ((perms & std::filesystem::perms::owner_exec) != std::filesystem::perms::none ||
          (perms & std::filesystem::perms::group_exec) != std::filesystem::perms::none ||
          (perms & std::filesystem::perms::others_exec) != std::filesystem::perms::none) {
        return program;
      }
    }
  }
  return {};
}

template <size_t buffer_size = 4096>
[[nodiscard]] std::optional<int> exec(const std::string &cmd, std::ostream &out) {
  auto pipe = popen((cmd + " 2>&1").c_str(), "r");
  if (!pipe) { return {}; }
  std::array<char, buffer_size> buffer{};
  while (fgets(buffer.data(), static_cast<int>(buffer.size()), pipe) != nullptr)
    out << buffer.data();
  return pclose(pipe);
}

} // namespace sv
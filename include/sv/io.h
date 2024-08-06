#pragma once

#include <filesystem>
#include <iosfwd>
#include <string>

namespace sv {

template <typename FS = std::fstream> std::string readFile(const std::filesystem::path &file) {
  FS t(file, std::ios::in);
  t.exceptions(std::ios::badbit | std::ios::failbit);
  std::stringstream buffer;
  buffer << t.rdbuf();
  return buffer.str();
}

} // namespace sv
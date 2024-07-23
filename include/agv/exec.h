#pragma once

#include <array>
#include <iosfwd>
#include <optional>

namespace agv {

template <size_t buffer_size = 4096>
[[nodiscard]] std::optional<int> exec(const std::string &cmd, std::ostream &out) {
  auto pipe = popen(cmd.c_str(), "r");
  if (!pipe) { return {}; }
  std::array<char, buffer_size> buffer{};
  while (fgets(buffer.data(), static_cast<int>(buffer.size()), pipe) != nullptr)
    out << buffer.data();
  return pclose(pipe);
}

} // namespace agv
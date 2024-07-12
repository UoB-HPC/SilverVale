#pragma once

#include <regex>

namespace agv {
std::regex globToRegex(const std::string &str, bool extended = true, bool globStar = false);
}

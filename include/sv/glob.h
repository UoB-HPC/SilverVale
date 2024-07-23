#pragma once

#include <regex>

namespace sv {
std::regex globToRegex(const std::string &str, bool extended = true, bool globStar = false);
}

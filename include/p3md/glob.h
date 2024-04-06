#pragma once

#include <regex>
namespace p3md {
std::regex globToRegex(const std::string &str, bool extended = false, bool globStar = false);
}

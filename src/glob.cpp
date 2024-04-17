#include "p3md/glob.h"
#include <regex>
#include <string>

// This is a direct port of https://github.com/fitzgen/glob-to-regexp from JS to C++
std::regex p3md::globToRegex(const std::string &str, bool extended, bool globStar) {

  std::string reStr;
  bool inGroup = false;

  for (size_t i = 0; i < str.length(); i++) {
    char c = str[i];

    switch (c) {
      case '/':
      case '$':
      case '^':
      case '+':
      case '.':
      case '(':
      case ')':
      case '=':
      case '!':
      case '|':
        reStr += "\\";
        reStr += c;
        break;
      case '?':
        if (extended) {
          reStr += ".";
          break;
        }
      case '[':
      case ']':
        if (extended) {
          reStr += c;
          break;
        }

      case '{':
        if (extended) {
          inGroup = true;
          reStr += "(";
          break;
        }

      case '}':
        if (extended) {
          inGroup = false;
          reStr += ")";
          break;
        }

      case ',':
        if (inGroup) {
          reStr += "|";
          break;
        }
        reStr += "\\";
        reStr += c;
        break;

      case '*': {
        if (!globStar) {
          // globstar is disabled, so treat any number of "*" as one
          reStr += ".*";
        } else {
          // Move over all consecutive "*"'s.
          // Also store the previous and next characters
          auto prevChar = str[i - 1];
          auto starCount = 1;
          while (str[i + 1] == '*') {
            starCount++;
            i++;
          }
          auto nextChar = str[i + 1];

          // globstar is enabled, so determine if this is a globstar segment
          auto isGlobstar = starCount > 1         // multiple "*"'s
                            && (prevChar == '/')  // from the start of the segment
                            && (nextChar == '/'); // to the end of the segment

          if (isGlobstar) {
            // it's a globstar, so match zero or more path segments
            reStr += "((?:[^/]*(?:\\/|$))*)";
            i++; // move over the "/"
          } else {
            // it's not a globstar, so only match one path segment
            reStr += "([^/]*)";
          }
        }
        break;
      }
      default: reStr += c;
    }
  }

  reStr.insert(0, 1, '^');
  reStr += "$";

  return std::regex(reStr);
}
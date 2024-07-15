#include <cmath>
#include <string>

std::string foo() { return "bar"; }
extern "C" double baz() { return std::fmin(1, 2.0); }

int main() {
  std::printf("%s %f\n", foo().c_str(), baz());
  return EXIT_SUCCESS;
}

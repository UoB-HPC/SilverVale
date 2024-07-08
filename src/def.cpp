#include <fstream>

#include "p3md/database.h"
#include "p3md/def.h"
#include "p3md/lua.h"

template <typename... Ts> struct TypeList {};

int p3md::def::run(const p3md::def::Options &options) {
  auto state = lua::teal(lua::Types);
  if (options.output.empty()) {
    teal::render(std::cout, state);
  } else {
    std::ofstream file(options.output, std::ios::trunc);
    teal::render(file, state);
  }
  return EXIT_SUCCESS;
}

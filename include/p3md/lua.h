#pragma once

#include "database.h"
#include "diff.h"

namespace p3md::lua {

using namespace aspartame;
template <typename... Ts> struct TypeList {};

static sol::state createState();

constexpr TypeList<             //
    p3md::Database,             //
    p3md::Database::Entry,      //
    p3md::Database::Bitcode,    //
    p3md::Database::Dependency, //
    p3md::Tree,                 //
    p3md::Source,               //
    p3md::Unit,                 //
    p3md::Codebase,             //
    p3md::Diff                  //
    >
    Types;

template <typename... Ts> [[nodiscard]] p3md::teal::State teal(TypeList<Ts...>) {
  p3md::teal::State state;
  (Ts::teal(state), ...);
  return state;
}

template <typename... Ts> void bind(sol::state &state, TypeList<Ts...>) { (Ts::bind(state), ...); }

static sol::state createState() {
  sol::state state;
  state.open_libraries(sol::lib::base,      //
                       sol::lib::package,   //
                       sol::lib::coroutine, //
                       sol::lib::string,    //
                       sol::lib::os,        //
                       sol::lib::math,      //
                       sol::lib::table,     //
                       sol::lib::debug,     //
                       sol::lib::bit32,     //
                       sol::lib::io,        //
                       sol::lib::utf8);
  bind(state, Types);
  return state;
}

} // namespace p3md::lua
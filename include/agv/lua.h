#pragma once

#include <functional>
#include <iosfwd>
#include <map>
#include <tuple>
#include <type_traits>

#include "sol/sol.hpp"

namespace agv::lua {

namespace {

template <typename> constexpr bool is_tuple = false;
template <typename... Args> constexpr bool is_tuple<std::tuple<Args...>> = true;

template <typename> constexpr bool is_function = false;
template <typename R, typename... Args>
constexpr bool is_function<std::function<R(Args...)>> = true;

template <typename> constexpr bool is_pair = false;
template <typename T1, typename T2> constexpr bool is_pair<std::pair<T1, T2>> = true;

template <typename> constexpr bool is_optional = false;
template <typename T> constexpr bool is_optional<std::optional<T>> = true;

template <typename> constexpr bool is_shared_ptr = false;
template <typename T> constexpr bool is_shared_ptr<std::shared_ptr<T>> = true;

template <typename> constexpr bool is_vector = false;
template <typename T> constexpr bool is_vector<std::vector<T>> = true;

template <typename> constexpr bool is_nested = false;
template <typename T> constexpr bool is_nested<sol::nested<T>> = true;

template <typename> constexpr bool is_table_t = false;
template <typename T> constexpr bool is_table_t<sol::as_table_t<T>> = true;

template <typename, typename = void> constexpr bool is_map_like = false;
template <typename T>
constexpr bool is_map_like<
    T, std::void_t<typename T::key_type, typename T::mapped_type,
                   decltype(std::declval<T &>()[std::declval<const typename T::key_type &>()])>> =
    true;

template <typename Func> struct function_traits;
template <typename R, typename... Args> struct function_traits<std::function<R(Args...)>> {
  using return_type = R;
  using args_tuple = std::tuple<Args...>;
};

} // namespace

using TealTpeLUT = std::function<std::string(const std::string &)>;
using TealTypeShow =
    std::function<void(std::ostream &, const std::string &, const TealTpeLUT &lut)>;
using TealTypeState = std::map<std::string, std::pair<std::string, TealTypeShow>>;

inline std::string showTealFn(const std::string &rtn, const std::vector<std::string> &args) {
  std::stringstream ss;
  ss << "(function(";
  for (size_t i = 0; i < args.size(); ++i) {
    if (args[i].empty()) continue;
    ss << args[i] << (i < args.size() - 1 && !args[i + 1].empty() ? ", " : "");
  }
  ss << "): " << rtn << ")";
  return ss.str();
}

template <typename Tuple, size_t x, size_t... xs>
constexpr auto sub_tail_tuple(const Tuple &t, std::index_sequence<x, xs...>) {
  return std::make_tuple(std::get<xs>(t)...);
}

template <typename Tuple> constexpr auto tuple_tail(const Tuple &t) {
  constexpr size_t tuple_size = std::tuple_size<Tuple>::value;
  return sub_tail_tuple(t, std::make_index_sequence<tuple_size>());
}

template <typename T> constexpr std::string showTealType(const TealTpeLUT &lut) {
  using U = std::decay_t<T>;
  if constexpr (std::is_same_v<sol::this_state, U> || std::is_same_v<sol::this_environment, U>) {
    return "";
  }
  if constexpr (std::is_void_v<U>) {
    return "nil";
  } else if constexpr (std::is_same_v<bool, U>) {
    return "boolean";
  } else if constexpr (std::is_same_v<std::string, U>) {
    return "string";
  } else if constexpr (std::is_floating_point_v<U>) {
    return "number";
  } else if constexpr (std::is_arithmetic_v<U>) {
    return "integer";
  } else if constexpr (is_table_t<U>) {
    return showTealType<decltype(std::declval<U>().value())>(lut);
  } else if constexpr (is_nested<U>) {
    return showTealType<typename U::nested_type>(lut);
  } else if constexpr (is_shared_ptr<U>) {
    return showTealType<typename U::element_type>(lut);
  } else if constexpr (is_pair<U>) {
    return "{" + showTealType<typename U::first_type>(lut) + ", " +
           showTealType<typename U::second_type>(lut) + "}";
  } else if constexpr (is_tuple<U>) {
    if constexpr (std::tuple_size_v<U> == 0) return "";
    else {
      using Tail = decltype(std::apply(
          [](auto &&, auto &&...xs) { return std::make_tuple(std::forward<decltype(xs)>(xs)...); },
          std::forward<U>(std::declval<U>())));
      auto tail = showTealType<Tail>(lut);
      return showTealType<std::tuple_element_t<0, U>>(lut) + (tail.empty() ? "" : ", " + tail);
    }
  } else if constexpr (is_vector<U>) {
    return "{" + showTealType<typename U::value_type>(lut) + "}";
  } else if constexpr (is_map_like<U>) {
    using K = typename U::key_type;
    using V = typename U::mapped_type;
    return "{" + showTealType<K>(lut) + ":" + showTealType<V>(lut) + "}";
  } else if constexpr (is_function<U>) {
    using F = function_traits<U>;
    return showTealFn(showTealType<typename F::return_type>(lut),
                      {showTealType<typename F::args_tuple>(lut)});
  } else {
    return lut(typeid(U).name());
  }
}
template <typename C, typename R> auto showTealType(R C::*, const TealTpeLUT &lut) -> std::string {
  return showTealType<R>(lut);
};
template <typename R, typename... Args>
auto showTealType(R (*)(Args...), const TealTpeLUT &lut) -> std::string {
  return showTealFn(showTealType<R>(lut), {showTealType<Args>(lut)...});
};
template <typename C, typename R, typename... Args>
auto showTealType(R (C::*)(Args...), const TealTpeLUT &lut) -> std::string {
  return showTealFn(showTealType<R>(lut), {showTealType<C>(lut), showTealType<Args>(lut)...});
};
template <typename C, typename R, typename... Args>
auto showTealType(R (C::*)(Args...) const, const TealTpeLUT &lut) -> std::string {
  return showTealFn(showTealType<R>(lut), {showTealType<C>(lut), showTealType<Args>(lut)...});
};

inline void showTealType(std::ostream &os, const TealTypeState &state) {
  TealTpeLUT lut = [&](auto &s) {
    if (auto it = state.find(s); it != state.end()) return it->second.first;
    else return s;
  };
  for (auto &[x, v] : state) {
    v.second(os, v.first, lut);
  }
}

template <typename Class, typename... Args>
void bindTeal(TealTypeState &out, const char *name, Args... args) {
  out.emplace(
      typeid(std::decay_t<Class>).name(),
      std::pair{name,
                [=](std::ostream &os, const std::string &name, const TealTpeLUT &lut) -> void {
                  os << "global record " << name << "\n";
                  (
                      [&]() {
                        os << "  " << std::get<0>(args) << ": "
                           << showTealType(std::get<1>(args), lut) << "\n";
                      }(),
                      ...);
                  os << "end" << std::endl;
                }});
}

template <typename... Ts> struct TypeList {};

template <typename... Ts> [[nodiscard]] agv::lua::TealTypeState bindTeal(TypeList<Ts...>) {
  agv::lua::TealTypeState state;
  (Ts::bindTeal(state), ...);
  return state;
}

template <typename... Ts> sol::state bindSol(TypeList<Ts...>) { //
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
  (Ts::bindSolUT(state), ...);
  return state;
}

template <typename C, typename... Args>
sol::usertype<C> bindSolUT(sol::state &s, const char *name, Args... args) {
  auto tpe = s.new_usertype<C>(name);
  (
      [&]() {
        std::string name = std::get<0>(args);
        if (!name.empty() && name.back() == '_') name.pop_back();
        tpe[name] = std::get<2>(args);
      }(),
      ...);
  return tpe;
}

} // namespace agv::lua

#ifdef SOL_UT_FN0
  #undef SOL_UT_FN0
#endif
#ifdef SOL_UT_FN
  #undef SOL_UT_FN
#endif
#ifdef SOL_UT_FN_ACC
  #undef SOL_UT_FN_ACC
#endif
#ifdef SOL_UT_RO
  #undef SOL_UT_RO
#endif
#ifdef DEF_SOL_UT_ACCESSOR
  #undef DEF_SOL_UT_ACCESSOR
#endif
#ifdef DEF_TEAL
  #undef DEF_TEAL
#endif
#ifdef DEF_SOL_UT
  #undef DEF_SOL_UT
#endif
#ifdef DEF_TEAL_SOL_UT
  #undef DEF_TEAL_SOL_UT
#endif

#define SOL_UT_FN0(Class, name, tpe)                                                               \
  std::tuple { #name, (tpe) & Class::name, (tpe) & Class::name }
#define SOL_UT_FN(Class, name)                                                                     \
  std::tuple { #name, &Class::name, &Class::name }
#define SOL_UT_FN_ACC(Class, name)                                                                 \
  std::tuple { #name, &Class::name##_, &Class::name##_ }
#define SOL_UT_RO(Class, name)                                                                     \
  std::tuple { #name, &Class::name, sol::readonly(&Class::name) }

#define DEF_SOL_UT_ACCESSOR(field)                                                                 \
  sol::as_table_t<decltype(field)> field##_() const { return field; }

#define DEF_TEAL(Class, ...)                                                                       \
  static void bindTeal(agv::lua::TealTypeState &state) {                                           \
    ::agv::lua::bindTeal<Class>(state, #Class, __VA_ARGS__);                                       \
  }

#define DEF_SOL_UT(Class, ...)                                                                     \
  static sol::usertype<Class> bindSolUT(sol::state &lua) {                                         \
    return ::agv::lua::bindSolUT<Class>(lua, #Class, __VA_ARGS__);                                 \
  }

#define DEF_TEAL_SOL_UT(Class, ...)                                                                \
  DEF_SOL_UT(Class, __VA_ARGS__)                                                                   \
  DEF_TEAL(Class, __VA_ARGS__)

#pragma once

#include <functional>
#include <iosfwd>
#include <map>
#include <tuple>
#include <type_traits>

#include "sol.hpp"

namespace p3md::teal {

template <typename> constexpr bool is_tuple = false;
template <typename... Args> constexpr bool is_tuple<std::tuple<Args...>> = true;

template <typename> constexpr bool is_function = false;
template <typename R, typename... Args>
constexpr bool is_function<std::function<R(Args...)>> = true;

template <typename> constexpr bool is_pair = false;
template <typename T1, typename T2> constexpr bool is_pair<std::pair<T1, T2>> = true;

template <typename> constexpr bool is_optional = false;
template <typename T> constexpr bool is_optional<std::optional<T>> = true;

template <typename> constexpr bool is_vector = false;
template <typename T> constexpr bool is_vector<std::vector<T>> = true;

template <typename> constexpr bool is_nested = false;
template <typename T> constexpr bool is_nested<sol::nested<T>> = true;

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

using LUT = std::function<std::string(const std::string &)>;
using Renderer = std::function<void(std::ostream &, const std::string &, const LUT &lut)>;
using State = std::map<std::string, std::pair<std::string, Renderer>>;

inline std::string renderFn(const std::string &rtn, const std::vector<std::string> &args) {
  std::stringstream ss;
  ss << "function(";
  for (size_t i = 0; i < args.size(); ++i) {
    if (args[i].empty()) continue;
    ss << args[i] << (i < args.size() - 1 && !args[i + 1].empty() ? ", " : "");
  }
  ss << "): " << rtn;
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

template <typename T> constexpr std::string render(const LUT &lut) {
  using U = std::decay_t<T>;
  if constexpr (std::is_same_v<sol::this_state, U> || std::is_same_v<sol::this_environment, U>) {
    return "";
  }if constexpr (std::is_void_v<  U>) {
    return "nil";
  } else if constexpr (std::is_same_v<bool, U>) {
    return "boolean";
  } else if constexpr (std::is_same_v<std::string, U>) {
    return "string";
  } else if constexpr (std::is_arithmetic_v<U>) {
    return "integer";
  } else if constexpr (std::is_floating_point_v<U>) {
    return "number";
  } else if constexpr (is_nested<U>) {
    return render<typename U::nested_type>(lut);
  } else if constexpr (is_tuple<U>) {
    if constexpr (std::tuple_size_v<U> == 0) return "";
    else {
      using Tail = decltype(std::apply(
          [](auto &&, auto &&...xs) { return std::make_tuple(std::forward<decltype(xs)>(xs)...); },
          std::forward<U>(std::declval<U>())));
      auto tail = render<Tail>(lut);
      return render<std::tuple_element_t<0, U>>(lut) + (tail.empty() ? "" : ", ");
    }
  } else if constexpr (is_vector<U>) {
    return "{" + render<typename U::value_type>(lut) + "}";
  } else if constexpr (is_map_like<U>) {
    using K = typename U::key_type;
    using V = typename U::mapped_type;
    return "{" + render<K>(lut) + ":" + render<V>(lut) + "}";
  } else if constexpr (is_function<U>) {
    using F = function_traits<U>;
    return renderFn(render<typename F::return_type>(lut), {render<typename F::args_tuple>(lut)});
  } else {
    return lut(typeid(U).name());
  }
}
template <typename C, typename R> auto render(R C::*, const LUT &lut) -> std::string {
  return render<R>(lut);
};
template <typename R, typename... Args> auto render(R (*)(Args...), const LUT &lut) -> std::string {
  return renderFn(render<R>(lut), {render<Args>(lut)...});
};
template <typename C, typename R, typename... Args>
auto render(R (C::*)(Args...), const LUT &lut) -> std::string {
  return renderFn(render<R>(lut), {render<C>(lut), render<Args>(lut)...});
};
template <typename C, typename R, typename... Args>
auto render(R (C::*)(Args...) const, const LUT &lut) -> std::string {
  return renderFn(render<R>(lut), {render<C>(lut), render<Args>(lut)...});
};

template <typename Class, typename... Args> void bind(State &out, const char *name, Args... args) {
  out.emplace(
      typeid(std::decay_t<Class>).name(),
      std::pair{name, [=](std::ostream &os, const std::string &name, const LUT &lut) -> void {
                  os << "global record " << name << "\n";
                  (
                      [&]() {
                        os << "  " << std::get<0>(args) << ": " << render(std::get<1>(args), lut)
                           << "\n";
                      }(),
                      ...);
                  os << "end" << std::endl;
                }});
}

inline void render(std::ostream &os, const State &state) {
  LUT lut = [&](auto &s) {
    if (auto it = state.find(s); it != state.end()) return it->second.first;
    else return s;
  };
  for (auto &[x, v] : state) {
    v.second(os, v.first, lut);
  }
}

} // namespace p3md::teal

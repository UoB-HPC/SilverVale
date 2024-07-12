cmake_minimum_required(VERSION 3.19 FATAL_ERROR)
project(SilverVale C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_C_STANDARD 11)

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_C_EXTENSIONS OFF)

include(FetchContent)

if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.24.0")
    cmake_policy(SET CMP0135 NEW)
endif ()

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)
find_package(TBB REQUIRED CONFIG)

message(STATUS "Found LLVM: ${LLVM_PACKAGE_VERSION}")
message(STATUS "Found Clang: ${Clang_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Using ClangConfig.cmake in: ${Clang_DIR}")
include(${LLVM_DIR}/AddLLVM.cmake)
include(${Clang_DIR}/AddClang.cmake)


set(ZSTD_BUILD_STATIC ON)
set(ZSTD_BUILD_SHARED OFF)
FetchContent_Declare(zstd
        URL https://github.com/facebook/zstd/releases/download/v1.5.6/zstd-1.5.6.tar.gz
        SOURCE_SUBDIR build/cmake)
FetchContent_MakeAvailable(zstd)

FetchContent_Declare(Aspartame
        #        SOURCE_DIR /home/tom/Aspartame
        GIT_REPOSITORY https://github.com/tom91136/Aspartame.git
        GIT_TAG 31a0e22298b0c89455f8edfbe48675c6858c2420)
FetchContent_MakeAvailable(Aspartame)

FetchContent_Declare(Catch2
        GIT_REPOSITORY https://github.com/catchorg/Catch2.git
        GIT_TAG v3.6.0)
FetchContent_MakeAvailable(Catch2)

FetchContent_Declare(json
        URL https://github.com/nlohmann/json/releases/download/v3.11.2/json.hpp
        URL_HASH SHA256=665fa14b8af3837966949e8eb0052d583e2ac105d3438baba9951785512cf921
        DOWNLOAD_NO_EXTRACT YES)
FetchContent_MakeAvailable(json)

FetchContent_Declare(tree-similarity
        URL https://github.com/DatabaseGroup/tree-similarity/archive/refs/tags/0.1.1.tar.gz
        URL_HASH SHA256=faefacde070ed6ddfbb843ff44c9a95290d195b3deb3b72f159688cadb53d891)
FetchContent_MakeAvailable(tree-similarity)

FetchContent_Declare(dtl
        URL https://github.com/cubicdaiya/dtl/archive/refs/tags/v1.20.tar.gz
        CONFIGURE_COMMAND ""
        BUILD_COMMAND "")
FetchContent_MakeAvailable(dtl)
add_library(dtl INTERFACE)
target_include_directories(dtl INTERFACE "${dtl_SOURCE_DIR}")

FetchContent_Declare(simplecpp
        GIT_REPOSITORY https://github.com/danmar/simplecpp.git
        GIT_TAG d90401601af4d7a747ce2b157db1f2d517467333)
FetchContent_MakeAvailable(simplecpp)

FetchContent_Declare(tree-sitter
        URL https://github.com/tree-sitter/tree-sitter/archive/refs/tags/v0.22.2.tar.gz
        URL_HASH SHA256=0c829523b876d4a37e1bd46a655c133a93669c0fe98fcd84972b168849c27afc
        CONFIGURE_COMMAND ""
        BUILD_COMMAND "")
FetchContent_MakeAvailable(tree-sitter)
add_library(tree-sitter
        OBJECT "${tree-sitter_SOURCE_DIR}/lib/src/lib.c")
target_include_directories(tree-sitter
        PRIVATE "${tree-sitter_SOURCE_DIR}/lib/src"
        PUBLIC "${tree-sitter_SOURCE_DIR}/lib/include")


# =====================

function(add_tree_sitter_lang repo lang kind value)
    set(target "tree-sitter-${lang}")
    if (kind STREQUAL "HASH")
        set(url "https://github.com/${repo}/${target}/archive/${value}.tar.gz")
    elseif (kind STREQUAL "TAG")
        set(url "https://github.com/${repo}/${target}/archive/refs/tags/${value}.tar.gz")
    else ()
        message(FATAL_ERROR "Unsupported kind ${kind} in ${item}")
    endif ()
    message("Adding: ${repo}/${target} from ${url}")
    FetchContent_Declare(${target} URL "${url}" CONFIGURE_COMMAND "" BUILD_COMMAND "")
    FetchContent_MakeAvailable(${target})
    file(WRITE "${${target}_BINARY_DIR}/include/tree_sitter_${lang}/api.h"
            " // Generated header, do not edit!
#include \"tree_sitter/parser.h\"
#ifdef __cplusplus
extern \"C\" {
#endif
TSLanguage * tree_sitter_${lang}();
#ifdef __cplusplus
}
#endif
")
    file(GLOB sources "${${target}_SOURCE_DIR}/src/*.c")
    add_library(${target} OBJECT ${sources})
    target_include_directories(${target} PUBLIC
            "${${target}_SOURCE_DIR}/src"
            "${${target}_BINARY_DIR}/include")
endfunction()

add_tree_sitter_lang(tree-sitter c TAG v0.20.7)
add_tree_sitter_lang(tree-sitter cpp TAG v0.20.5)
add_tree_sitter_lang(tree-sitter julia TAG v0.22.0)
add_tree_sitter_lang(tree-sitter rust TAG v0.21.2)
add_tree_sitter_lang(stadelmanma fortran HASH f73d473)


# =====================

set(SOL_VERSION "v3.3.0")

FetchContent_Declare(sol2-sol
        URL https://github.com/ThePhD/sol2/releases/download/${SOL_VERSION}/sol.hpp
        URL_HASH SHA256=e095a961a5189863745e6c101124fce944af991f3d4726a1e82c5b4a885a187f
        DOWNLOAD_NO_EXTRACT YES)
FetchContent_MakeAvailable(sol2-sol)

FetchContent_Declare(sol2-forward
        URL https://github.com/ThePhD/sol2/releases/download/${SOL_VERSION}/forward.hpp
        URL_HASH SHA256=8fc34d74e9b4b8baa381f5e6ab7b6f6b44114cd355c718505495943ff6b85740
        DOWNLOAD_NO_EXTRACT YES)
FetchContent_MakeAvailable(sol2-forward)

FetchContent_Declare(lua
        URL https://www.lua.org/ftp/lua-5.4.7.tar.gz
        URL_HASH SHA256=9fbf5e28ef86c69858f6d3d34eccc32e911c1a28b4120ff3e84aaa70cfbf1e30
)
FetchContent_MakeAvailable(lua)

add_library(sol2 INTERFACE)
target_include_directories(sol2 INTERFACE ${sol2-sol_SOURCE_DIR} ${sol2-forward_SOURCE_DIR})

file(GLOB LUA_SOURCES ${lua_SOURCE_DIR}/src/*.c)
add_library(lua ${LUA_SOURCES})
target_include_directories(lua PUBLIC ${lua_SOURCE_DIR}/src)
target_compile_definitions(lua PUBLIC LUA_USE_LINUX LUA_COMPAT_5_3)

# =====================


list(APPEND AGV_COMPILE_DEBUG_FLAGS -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer)
list(APPEND AGV_COMPILE_RELEASE_FLAGS -O3 -march=native)
list(APPEND AGV_LINK_DEBUG_FLAGS -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer)
list(APPEND AGV_COMPILE_FLAGS
        "$<$<CONFIG:Release>:${AGV_COMPILE_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${AGV_COMPILE_DEBUG_FLAGS}>"
        -fexceptions
        -Wall
        -Wextra
        -Werror=return-type
        -Werror=switch
        #        -Wthread-safety
        -Wcast-align
        -Wno-type-limits

        -Wno-unused-parameter
        -Wno-unused-function
        -Wno-unused-variable
        -Wno-unknown-warning-option
)
list(APPEND AGV_LINK_FLAGS "$<$<CONFIG:Debug>:${AGV_LINK_DEBUG_FLAGS}>")

add_library(AGV
        src/tree.cpp
        src/tool_script.cpp
        src/tool_delta.cpp
        src/tool_index.cpp
        src/tool_inspect.cpp
        src/diff.cpp
        src/cli.cpp
        src/database.cpp
        src/compress.cpp
        src/glob.cpp)
target_include_directories(AGV PRIVATE include ${json_SOURCE_DIR} ${simplecpp_SOURCE_DIR})
target_link_libraries(AGV

        PRIVATE
        libzstd_static
        Aspartame::Aspartame
        TreeSimilarity
        dtl
        TBB::tbb
        simplecpp_obj
        sol2
        lua

        PUBLIC
        tree-sitter
        tree-sitter-c
        tree-sitter-cpp
        tree-sitter-julia
        tree-sitter-rust
        tree-sitter-fortran
        clang-cpp
        LLVM
)


target_compile_options(AGV PRIVATE ${AGV_COMPILE_FLAGS})
target_link_options(AGV PRIVATE ${AGV_LINK_FLAGS})
target_precompile_headers(AGV PRIVATE
        [["clang/Tooling/Tooling.h"]]
        [["clang/Frontend/ASTUnit.h"]]
        [["clang/AST/RecursiveASTVisitor.h"]]
        [["clang/Frontend/FrontendActions.h"]]
        [["clang/Tooling/CommonOptionsParser.h"]]

        [["llvm/IR/Module.h"]]
        [["llvm/IR/LLVMContext.h"]]
        [["llvm/Support/raw_ostream.h"]]
        [["llvm/Bitcode/BitcodeReader.h"]]

        [["oneapi/tbb.h"]]

        [["json.hpp"]]
#        [["lua.hpp"]]
        [["sol.hpp"]]
        [["zstd.h"]]

        [["aspartame/map.hpp"]]
        [["aspartame/optional.hpp"]]
        [["aspartame/string.hpp"]]
        [["aspartame/vector.hpp"]]
        [["aspartame/view.hpp"]]

        <map>
        <tuple>
        <ctime>
        <cstdint>
        <memory>
        <regex>
        <string>
        <vector>
        <utility>
        <optional>
        <iostream>
        <type_traits>
        <functional>
        <unordered_map>

)

add_executable(driver src/driver.cpp)
add_executable(tests test/tree.cpp test/index.cpp)

target_link_libraries(driver PRIVATE Aspartame::Aspartame)
target_link_libraries(tests PRIVATE Aspartame::Aspartame Catch2::Catch2WithMain)

set_target_properties(driver PROPERTIES OUTPUT_NAME "agv")

execute_process(
        COMMAND clang -print-resource-dir
        OUTPUT_VARIABLE CLANG_RESOURCE_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE
        RESULT_VARIABLE CLANG_COMMAND_RESULT
)

if (CLANG_COMMAND_RESULT EQUAL 0)
    message(STATUS "Found Clang resource directory: ${CLANG_RESOURCE_DIR}")
    set(CLANG_RESOURCE_DIR "${CLANG_RESOURCE_DIR}/include")
else ()
    message(WARNING "Failed to get clang resource directory (${CLANG_COMMAND_RESULT}), index tests may not pass.")
    set(CLANG_RESOURCE_DIR "(call to clang -print-resource-dir failed)")
endif ()

set(FIXTURE_MICROSTREAM_DIR ${CMAKE_BINARY_DIR}/microstream_build)
set(FIXTURE_TMP_DIR ${CMAKE_BINARY_DIR})
configure_file(test/fixture/fixture.h.in ${CMAKE_CURRENT_BINARY_DIR}/fixture.h @ONLY)


add_custom_command(TARGET tests
        PRE_BUILD
        COMMAND ${CMAKE_COMMAND} -E env CC=clang CXX=clang++ CXXFLAGS="-save-temps=obj" ${CMAKE_COMMAND}
        -S "${CMAKE_SOURCE_DIR}/test/fixture/microstream"
        -B "${FIXTURE_MICROSTREAM_DIR}"
        -DUSE_OMP=ON
        -DCMAKE_BUILD_TYPE=Release
        -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
        -DCMAKE_VERBOSE_MAKEFILE=ON
        -DCMAKE_CXX_LINK_EXECUTABLE='echo "" '
        COMMAND ${CMAKE_COMMAND} --build "${FIXTURE_MICROSTREAM_DIR}"
        BYPRODUCTS "${FIXTURE_MICROSTREAM_DIR}/CMakeCache.txt"
        WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
        COMMENT "Configure microstream fixture"
)
target_include_directories(tests PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")


foreach (target driver tests)
    target_link_libraries(${target} PRIVATE AGV)
    target_include_directories(${target} PRIVATE include ${json_SOURCE_DIR})
    target_compile_options(${target} PRIVATE ${AGV_COMPILE_FLAGS})
    target_link_options(${target} PRIVATE ${AGV_LINK_FLAGS})
endforeach ()

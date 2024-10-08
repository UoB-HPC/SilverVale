cmake_minimum_required(VERSION 3.19 FATAL_ERROR)
project(SilverVale C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_C_STANDARD 11)

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

include(cmake/CPM.cmake)

if (CMAKE_VERSION VERSION_GREATER_EQUAL "3.24.0")
    cmake_policy(SET CMP0135 NEW)
endif ()

option(SV_ENABLE_CUDA_FIXTURES "Enable CUDA fixtures" ON)
option(SV_ENABLE_HIP_FIXTURES "Enable HIP fixtures" ON)

CPMAddPackage("gh:madler/zlib@1.3.1")
CPMAddPackage("gh:nlohmann/json@3.11.3")
CPMAddPackage("gh:catchorg/Catch2@3.6.0")
CPMAddPackage("gh:ThePhD/sol2@3.3.0")
CPMAddPackage("gh:DatabaseGroup/tree-similarity#0.1.1")

CPMAddPackage(
        NAME bxzstr
        GITHUB_REPOSITORY tmaklin/bxzstr
        VERSION 1.2.2
        OPTIONS "ZLIB_FOUND OFF" "BZIP2_FOUND OFF" "LIBLZMA_FOUND OFF" "ZSTD_FOUND ON" "CMAKE_BUILD_TESTS OFF"
)
CPMAddPackage(
        NAME fmt
        GITHUB_REPOSITORY fmtlib/fmt
        GIT_TAG 11.0.1
        OPTIONS "BUILD_SHARED_LIBS OFF"
)
CPMAddPackage(
        NAME spdlog
        GITHUB_REPOSITORY gabime/spdlog
        VERSION 1.14.1
        OPTIONS "SPDLOG_FMT_EXTERNAL ON" "BUILD_SHARED_LIBS OFF"
)
CPMAddPackage(
        NAME Aspartame
        GITHUB_REPOSITORY tom91136/Aspartame
        GIT_TAG 8e87a5bb)
CPMAddPackage(
        NAME zstd
        GITHUB_REPOSITORY facebook/zstd
        VERSION 1.5.6
        SOURCE_SUBDIR build/cmake
        OPTIONS "ZSTD_BUILD_STATIC ON" "ZSTD_BUILD_SHARED OFF" "BUILD_SHARED_LIBS OFF")
CPMAddPackage(
        NAME xxHash
        GITHUB_REPOSITORY Cyan4973/xxHash
        GIT_TAG a57f6cc # after https://github.com/Cyan4973/xxHash/commit/f17d319
        SOURCE_SUBDIR cmake_unofficial
        OPTIONS "XXHASH_BUILD_ENABLE_INLINE_API OFF" "XXHASH_BUILD_XXHSUM OFF")
CPMAddPackage(
        NAME lua
        GITHUB_REPOSITORY lua/lua
        VERSION 5.4.7
        DOWNLOAD_ONLY YES)
CPMAddPackage("gh:muellan/clipp@1.2.3")
if (clipp_ADDED)
    add_library(clipp INTERFACE IMPORTED)
    target_include_directories(clipp INTERFACE ${clipp_SOURCE_DIR}/include)
endif ()
CPMAddPackage(
        NAME BS_thread_pool
        GITHUB_REPOSITORY bshoshany/thread-pool
        VERSION 4.1.0)
if (BS_thread_pool_ADDED)
    add_library(BS_thread_pool INTERFACE)
    target_include_directories(BS_thread_pool INTERFACE "${BS_thread_pool_SOURCE_DIR}/include")
endif ()
if (lua_ADDED)
    file(GLOB lua_sources ${lua_SOURCE_DIR}/*.c)
    list(REMOVE_ITEM lua_sources "${lua_SOURCE_DIR}/lua.c" "${lua_SOURCE_DIR}/luac.c")
    add_library(lua STATIC ${lua_sources})
    target_compile_definitions(lua PUBLIC LUA_USE_LINUX LUA_COMPAT_5_3)
    target_include_directories(lua SYSTEM PUBLIC $<BUILD_INTERFACE:${lua_SOURCE_DIR}>)
endif ()
CPMAddPackage(
        NAME dtl
        GITHUB_REPOSITORY cubicdaiya/dtl
        VERSION 1.21
        DOWNLOAD_ONLY YES)
if (dtl_ADDED)
    add_library(dtl INTERFACE)
    target_include_directories(dtl INTERFACE "${dtl_SOURCE_DIR}")
endif ()
CPMAddPackage(
        NAME tree-sitter
        GITHUB_REPOSITORY tree-sitter/tree-sitter
        VERSION 0.22.2
        DOWNLOAD_ONLY YES)
if (tree-sitter_ADDED)
    add_library(tree-sitter
            OBJECT "${tree-sitter_SOURCE_DIR}/lib/src/lib.c")
    target_include_directories(tree-sitter
            PRIVATE "${tree-sitter_SOURCE_DIR}/lib/src"
            PUBLIC "${tree-sitter_SOURCE_DIR}/lib/include")
endif ()

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
    CPMAddPackage(NAME ${target} URL "${url}" CONFIGURE_COMMAND "" BUILD_COMMAND "")

    if (${target}_ADDED)

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
    endif ()
endfunction()

add_tree_sitter_lang(tree-sitter c TAG v0.20.7)
add_tree_sitter_lang(tree-sitter cpp TAG v0.20.5)
add_tree_sitter_lang(tree-sitter julia TAG v0.22.0)
add_tree_sitter_lang(tree-sitter rust TAG v0.21.2)
add_tree_sitter_lang(tree-sitter-grammars cuda TAG v0.20.5)
add_tree_sitter_lang(stadelmanma fortran HASH dde9829)


# =====================

add_library(dummy INTERFACE)
add_library(ZLIB::ZLIB ALIAS zlibstatic)
add_library(Zstd::Zstd ALIAS libzstd_static)
add_library(BZip2::BZip2 ALIAS dummy)
add_library(LibLZMA::LibLZMA ALIAS dummy)

list(APPEND SV_COMPILE_DEBUG_FLAGS -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer)
list(APPEND SV_LINK_DEBUG_FLAGS -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer)

list(APPEND SV_COMPILE_RELEASE_FLAGS -O3 -march=native)
list(APPEND SV_LINK_RELEASE_FLAGS -static-libgcc -static-libstdc++ LINKER:--gc-sections)

list(APPEND SV_COMPILE_FLAGS
        "$<$<CONFIG:Release>:${SV_COMPILE_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${SV_COMPILE_DEBUG_FLAGS}>"
        -fexceptions
        -Wall
        -Wextra
        -Werror=return-type
        -Werror=switch
        -Wcast-align
        -Wno-type-limits

        -Wno-unused-parameter
        -Wno-unused-function
        -Wno-unused-variable
        -Wno-unknown-warning-option
        #        -ftime-trace
)
list(APPEND SV_LINK_FLAGS
        "$<$<CONFIG:Release>:${SV_LINK_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${SV_LINK_DEBUG_FLAGS}>"
)

add_library(SilverVale STATIC

        src/tree_ts.cpp

        src/index_llvm.cpp
        src/index_gcc.cpp

        src/tool_script.cpp
        src/tool_delta.cpp
        src/tool_index.cpp
        src/tool_dump.cpp
        src/diff.cpp
        src/model.cpp
        src/glob.cpp)

target_include_directories(SilverVale PRIVATE include)
target_link_libraries(SilverVale
        PRIVATE
        Aspartame::Aspartame
        nlohmann_json::nlohmann_json
        TreeSimilarity
        dtl
        BS_thread_pool
        sol2
        lua
        fmt::fmt
        clipp
        spdlog::spdlog
        xxHash::xxhash
        zlibstatic # for GCOV
        libzstd_static
        bxzstr

        PUBLIC
        tree-sitter
        tree-sitter-c
        tree-sitter-cpp
        tree-sitter-cuda # cpp gives ERROR tokens for __$space__, <<<>>>, etc
        tree-sitter-julia
        tree-sitter-rust
        tree-sitter-fortran
)
target_compile_options(SilverVale PRIVATE ${SV_COMPILE_FLAGS})
target_link_options(SilverVale PRIVATE ${SV_LINK_FLAGS})
#set_property(TARGET SilverVale PROPERTY CXX_INCLUDE_WHAT_YOU_USE include-what-you-use)


if (CMAKE_BUILD_TYPE STREQUAL "Debug")
    target_precompile_headers(SilverVale PRIVATE
            [["nlohmann/json.hpp"]]
            #            [["fmt/core.h"]]
            #            [["fmt/std.h"]]
            [["spdlog/spdlog.h"]]
            [["clipp.h"]]
            [["sol/sol.hpp"]]
            [["zstd.h"]]

            [["aspartame/map.hpp"]]
            [["aspartame/set.hpp"]]
            [["aspartame/unordered_set.hpp"]]
            [["aspartame/unordered_map.hpp"]]
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
            <fstream>
            <type_traits>
            <functional>
            <filesystem>
            <unordered_map>
    )
endif ()

add_executable(driver src/driver.cpp)
add_executable(tests test/tree.cpp test/tstree.cpp test/index_microstream.cpp test/index_simple.cpp test/preprocessor.cpp)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)
set_target_properties(driver PROPERTIES OUTPUT_NAME "sv")
foreach (target driver tests)
    target_link_libraries(${target} PRIVATE SilverVale Aspartame::Aspartame nlohmann_json::nlohmann_json sol2 lua fmt::fmt spdlog::spdlog clipp bxzstr)
    target_include_directories(${target} PRIVATE include)
    target_compile_options(${target} PRIVATE ${SV_COMPILE_FLAGS})
    target_link_options(${target} PRIVATE ${SV_LINK_FLAGS})
    set_property(TARGET ${target} PROPERTY INTERPROCEDURAL_OPTIMIZATION ON)
endforeach ()

function(setup_fixture name cc cxx fc suffix)
    set(fixture_name "${name}_${cc}_${suffix}")
    string(TOUPPER ${fixture_name} fixture_name_upper)
    message(STATUS "Adding fixture ${fixture_name}")
    set(build_dir "${CMAKE_BINARY_DIR}/${fixture_name}_build")
    set(FIXTURE_${fixture_name_upper}_EXPR "std::tuple{ \"${name}\", \"${cc}\", \"${suffix}\", \"${build_dir}\" }" PARENT_SCOPE)

    add_custom_command(
            TARGET tests POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E rm -rf "${build_dir}"
            COMMAND ${CMAKE_COMMAND} -E env CCACHE_DISABLE=1 CC=${cc} CXX=${cxx} FC=${fc} ${CMAKE_COMMAND}
            -S "${CMAKE_SOURCE_DIR}/test/fixture/${name}"
            -B "${build_dir}"
            ${ARGN}
            -DCMAKE_BUILD_TYPE=Release
            -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
            -DCMAKE_VERBOSE_MAKEFILE=ON
            WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
            COMMENT "Configure fixture ${name} as ${fixture_name}"
    )
endfunction()

add_library(processor-fixture OBJECT test/fixture/processor.cpp)
target_compile_options(processor-fixture PRIVATE -E)
set(FIXTURE_PROCESSOR_FILE "$<TARGET_OBJECTS:processor-fixture>")

set(GCC_COVERAGE_FLAGS "-fprofile-arcs;-ftest-coverage;-g") # -g helps retain `return` in coverage (!?)
set(CLANG_COVERAGE_FLAGS "-fprofile-instr-generate;-fcoverage-mapping")

setup_fixture(simple clang clang++ flang-new cxx -DCMAKE_CXX_FLAGS="${CLANG_COVERAGE_FLAGS}")
setup_fixture(simple gcc g++ gfortran cxx -DCMAKE_CXX_FLAGS="${GCC_COVERAGE_FLAGS}")
setup_fixture(simple gcc g++ gfortran f90 -DUSE_FORTRAN=ON -DCMAKE_Fortran_FLAGS="${GCC_COVERAGE_FLAGS}")

setup_fixture(microstream gcc g++ gfortran serial_f90 -DUSE_SERIAL=ON -DUSE_FORTRAN=ON -DCMAKE_Fortran_FLAGS="${GCC_COVERAGE_FLAGS}")
setup_fixture(microstream gcc g++ gfortran omp_f90 -DUSE_OMP=ON -DUSE_FORTRAN=ON -DCMAKE_Fortran_FLAGS="${GCC_COVERAGE_FLAGS}")

setup_fixture(microstream clang clang++ flang-new serial -DUSE_SERIAL=ON -DCMAKE_CXX_FLAGS="${CLANG_COVERAGE_FLAGS}")
setup_fixture(microstream gcc g++ gfortran serial -DUSE_SERIAL=ON -DCMAKE_CXX_FLAGS="${GCC_COVERAGE_FLAGS}")

setup_fixture(microstream clang clang++ flang-new omp -DUSE_OMP=ON -DCMAKE_CXX_FLAGS="${CLANG_COVERAGE_FLAGS}")
setup_fixture(microstream gcc g++ gfortran omp -DUSE_OMP=ON -DCMAKE_CXX_FLAGS="${GCC_COVERAGE_FLAGS}")

setup_fixture(microstream clang clang++ flang-new omp_target -DUSE_OMP_TARGET=sm_60 -DCMAKE_CXX_FLAGS="${CLANG_COVERAGE_FLAGS}")

if (SV_ENABLE_HIP_FIXTURES)
    setup_fixture(microstream clang clang++ flang-new hip -DUSE_HIP=gfx1036 -DCMAKE_CXX_FLAGS="${CLANG_COVERAGE_FLAGS}")
endif ()
if (SV_ENABLE_CUDA_FIXTURES)
    setup_fixture(microstream clang clang++ flang-new cuda -DUSE_CUDA=sm_60 -DCMAKE_CUDA_COMPILER=clang++ -DCMAKE_CXX_FLAGS="${CLANG_COVERAGE_FLAGS}" -DCMAKE_CUDA_FLAGS="${CLANG_COVERAGE_FLAGS}")
endif ()

set(FIXTURE_TMP_DIR ${CMAKE_CURRENT_BINARY_DIR})


set(config_template "#pragma once\n")
get_cmake_property(all_variables VARIABLES)
foreach (var ${all_variables})
    if (var MATCHES "^FIXTURE_")
        if (var MATCHES "_EXPR$")
            string(APPEND config_template "#cmakedefine ${var} @${var}@\n")
        else ()
            string(APPEND config_template "#cmakedefine ${var} \"@${var}@\"\n")
        endif ()
    endif ()
endforeach ()
file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/fixture.h.in" "${config_template}")
configure_file("${CMAKE_CURRENT_BINARY_DIR}/fixture.h.in" "${CMAKE_CURRENT_BINARY_DIR}/fixture.h" @ONLY)
target_include_directories(tests PRIVATE "${CMAKE_CURRENT_BINARY_DIR}")
file(GENERATE # expand generator expressions too
        OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/fixture.h"
        INPUT "${CMAKE_CURRENT_BINARY_DIR}/fixture.h")


add_subdirectory(uproot-gcc)
add_subdirectory(uproot-clang)
add_dependencies(tests uproot-gcc uproot-clang processor-fixture)
add_dependencies(driver uproot-gcc uproot-clang)

target_compile_definitions(SilverVale PRIVATE UPROOT_CLANG_SO="$<TARGET_FILE_NAME:uproot-clang-system>")
target_compile_definitions(SilverVale PRIVATE UPROOT_GCC_SO="$<TARGET_FILE_NAME:uproot-gcc>")


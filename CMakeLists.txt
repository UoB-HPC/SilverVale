cmake_minimum_required(VERSION 3.19 FATAL_ERROR)
project(P3MD C CXX)

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
        GIT_TAG v3.4.0)
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

list(APPEND P3MD_COMPILE_DEBUG_FLAGS -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer)
list(APPEND P3MD_COMPILE_RELEASE_FLAGS -O3 -march=native)
list(APPEND P3MD_LINK_DEBUG_FLAGS -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer)
list(APPEND P3MD_COMPILE_FLAGS
        "$<$<CONFIG:Release>:${P3MD_COMPILE_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${P3MD_COMPILE_DEBUG_FLAGS}>"
        -fexceptions
        -Wall
        -Wextra
        -Werror=return-type
        -Werror=switch
        -Wthread-safety
        -Wcast-align
        -Wno-type-limits

        -Wno-unused-parameter
        -Wno-unused-function
        -Wno-unused-variable
)
list(APPEND P3MD_LINK_FLAGS "$<$<CONFIG:Debug>:${P3MD_LINK_DEBUG_FLAGS}>" )

add_library(P3MD
        src/tree.cpp
        src/diff.cpp
        src/dump.cpp
        src/build.cpp
        src/list.cpp
        src/p3md.cpp
        src/database.cpp
        src/compress.cpp
        src/glob.cpp)
target_include_directories(P3MD PRIVATE include ${json_SOURCE_DIR})
target_link_libraries(P3MD

        PRIVATE
        libzstd_static
        Aspartame::Aspartame
        TreeSimilarity
        dtl
        TBB::tbb

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


target_compile_options(P3MD PRIVATE ${P3MD_COMPILE_FLAGS})
target_link_options(P3MD PRIVATE ${P3MD_LINK_FLAGS})
target_precompile_headers(P3MD PRIVATE

        [["clang/AST/RecursiveASTVisitor.h"]]
        [["clang/Frontend/FrontendActions.h"]]
        [["clang/Tooling/CommonOptionsParser.h"]]
        [["clang/Tooling/Tooling.h"]]
        [["llvm/Support/CommandLine.h"]]

        [["aspartame/view.hpp"]]
        [["aspartame/vector.hpp"]]
        [["aspartame/optional.hpp"]]

        [["json.hpp"]]

        <optional>
        <iostream>
        <unordered_map>
        <utility>
        <vector>
        <string>
        <regex>
)

add_executable(driver src/driver.cpp)
add_executable(tests test/main.cpp)

target_link_libraries(driver PRIVATE Aspartame::Aspartame)
target_link_libraries(tests PRIVATE Aspartame::Aspartame Catch2::Catch2WithMain)

set_target_properties(driver PROPERTIES OUTPUT_NAME "p3md")

foreach (target driver tests)
    target_link_libraries(${target} PRIVATE P3MD)
    target_include_directories(${target} PRIVATE include ${json_SOURCE_DIR})
    target_compile_options(${target} PRIVATE ${P3MD_COMPILE_FLAGS})
    target_link_options(${target} PRIVATE ${P3MD_LINK_FLAGS})
endforeach ()

cmake_minimum_required(VERSION 3.19 FATAL_ERROR)
project(uproot_clang CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(SV_CLANG_INCLUDE_PATHS "" CACHE STRING "LLVM+Clang include paths for Clang uproot clang plugin")

set(SV_CLANG_INCLUDE_PATHS "/home/tom/Downloads/llvm-project-19.1.0-rc2.src/dest/include/")


option(SV_ENABLE_UPROOT_CLANG_TESTS "Enable tests for the Clang uproot plugin" ON)

list(APPEND UPROOT_CLANG_COMPILE_FLAGS
        "$<$<CONFIG:Release>:${SV_COMPILE_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${SV_COMPILE_DEBUG_FLAGS}>"

        -stdlib=libc++

        -fexceptions
        -Wall
        -Wextra
        -Werror=return-type
        -Werror=switch
        -Wno-type-limits

        -Wno-unused-parameter
        -Wno-unused-function
        -Wno-unused-variable

)
list(APPEND UPROOT_CLANG_LINK_FLAGS
        -stdlib=libc++

        "$<$<CONFIG:Release>:${SV_LINK_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${SV_LINK_DEBUG_FLAGS}>")

set(UPROOT_ASAN_FLAGS
        "$<$<AND:$<CXX_COMPILER_ID:Clang>,$<CONFIG:Debug>>:-shared-libasan>")

add_library(uproot_clang SHARED main.cpp compress.cpp tree_clangast.cpp tree_llvmir.cpp)
set_target_properties(uproot_clang PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
list(APPEND targets uproot_clang)


if (SV_ENABLE_UPROOT_CLANG_TESTS)
    find_package(LLVM REQUIRED CONFIG)
    find_package(Clang REQUIRED CONFIG)
    message(STATUS "Found LLVM: ${LLVM_PACKAGE_VERSION}")
    message(STATUS "Found Clang: ${Clang_VERSION}")

    add_executable(uproot_clang_tests test/ast.cpp)
    target_link_libraries(uproot_clang_tests PRIVATE Catch2::Catch2WithMain LLVM clang-cpp uproot_clang)
    list(APPEND targets uproot_clang_tests)
endif ()


foreach (target ${targets})
    target_compile_options(${target} PRIVATE ${UPROOT_CLANG_COMPILE_FLAGS} -fno-rtti "${UPROOT_ASAN_FLAGS}")
    target_link_options(${target} PRIVATE ${UPROOT_CLANG_LINK_FLAGS} "${UPROOT_ASAN_FLAGS}")
    target_link_libraries(${target} PRIVATE Aspartame::Aspartame fmt::fmt-header-only spdlog::spdlog_header_only nlohmann_json::nlohmann_json libzstd_static)
    target_include_directories(${target} PRIVATE BEFORE ${SV_CLANG_INCLUDE_PATHS} ${CMAKE_SOURCE_DIR}/include)
endforeach ()


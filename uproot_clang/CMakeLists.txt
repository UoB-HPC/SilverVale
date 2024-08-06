cmake_minimum_required(VERSION 3.19 FATAL_ERROR)
project(uproot_clang CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_library(uproot_clang SHARED main.cpp compress.cpp tree_clangast.cpp tree_llvmir.cpp)

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

#message(STATUS "Found LLVM: ${LLVM_PACKAGE_VERSION}")
#message(STATUS "Found Clang: ${Clang_VERSION}")
#message(STATUS "Using LLVMConfig.cmake in: ${LLVM_INCLUDE_DIR}")
#message(STATUS "Using ClangConfig.cmake in: ${Clang_INCLUDE_DIR}")
#include(${LLVM_DIR}/AddLLVM.cmake)
#include(${Clang_DIR}/AddClang.cmake)

list(APPEND UPROOT_CLANG_COMPILE_FLAGS
        "$<$<CONFIG:Release>:${SV_COMPILE_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${SV_COMPILE_DEBUG_FLAGS}>"
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
        "$<$<CONFIG:Release>:${SV_LINK_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${SV_LINK_DEBUG_FLAGS}>"
)


set(UPROOT_ASAN_FLAGS
        "$<$<AND:$<CXX_COMPILER_ID:Clang>,$<CONFIG:Debug>>:-shared-libasan>"
)
target_include_directories(uproot_clang PUBLIC ${GCC_PLUGIN_INCLUDE_DIR} ${CMAKE_SOURCE_DIR}/include)
target_compile_options(uproot_clang PRIVATE ${UPROOT_CLANG_COMPILE_FLAGS} -fno-rtti "${UPROOT_ASAN_FLAGS}")
target_link_options(uproot_clang PRIVATE ${UPROOT_CLANG_LINK_FLAGS} "${UPROOT_ASAN_FLAGS}")
target_link_libraries(uproot_clang PRIVATE Aspartame::Aspartame fmt::fmt nlohmann_json::nlohmann_json libzstd_static)


set_target_properties(uproot_clang PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")




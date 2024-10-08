cmake_minimum_required(VERSION 3.19 FATAL_ERROR)
project(uproot-gcc CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

add_library(uproot-gcc SHARED main.cpp tree_gimple.cpp)

execute_process(
        COMMAND gcc -print-file-name=plugin
        OUTPUT_VARIABLE GCC_PLUGIN_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE
        RESULT_VARIABLE GCC_PLUGIN_DIR_RESULT)

set(GCC_PLUGIN_WARNING_MESSAGE "you may need to install development packages (e.g gcc-plugin-devel) for GCC")
if (GCC_PLUGIN_DIR_RESULT EQUAL 0)
    message(STATUS "Found GCC plugin directory: ${GCC_PLUGIN_DIR}")
    set(GCC_PLUGIN_INCLUDE_DIR "${GCC_PLUGIN_DIR}/include")
    if (NOT EXISTS "${GCC_PLUGIN_INCLUDE_DIR}")
        message(WARNING "${GCC_PLUGIN_INCLUDE_DIR} does not exist, ${GCC_PLUGIN_WARNING_MESSAGE}")
    endif ()
else ()
    message(WARNING "Failed to get GCC plugin dir (${GCC_PLUGIN_DIR_RESULT}), ${GCC_PLUGIN_WARNING_MESSAGE}")
    set(GCC_PLUGIN_INCLUDE_DIR "(call to `gcc -print-file-name=plugin` failed)")
endif ()

list(APPEND UPROOT_GCC_COMPILE_FLAGS
        "$<$<CONFIG:Release>:${SV_COMPILE_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${SV_COMPILE_DEBUG_FLAGS}>"
        -fexceptions
        -Wall
        -Wextra
        -Werror=return-type
        -Werror=switch
        -Wno-type-limits

        -Wno-deprecated-enum-enum-conversion
        -Wno-unused-parameter
        -Wno-unused-function
        -Wno-unused-variable
        -Wno-unknown-warning-option
)
list(APPEND UPROOT_GCC_LINK_FLAGS
        "$<$<CONFIG:Release>:${SV_LINK_RELEASE_FLAGS}>"
        "$<$<CONFIG:Debug>:${SV_LINK_DEBUG_FLAGS}>"
)


set(UPROOT_ASAN_FLAGS
        "$<$<AND:$<CXX_COMPILER_ID:Clang>,$<CONFIG:Debug>>:-shared-libasan>"
)
target_include_directories(uproot-gcc PUBLIC . ${GCC_PLUGIN_INCLUDE_DIR} ${CMAKE_SOURCE_DIR}/include)
target_compile_options(uproot-gcc PRIVATE ${UPROOT_GCC_COMPILE_FLAGS} -fno-rtti "${UPROOT_ASAN_FLAGS}")
target_link_options(uproot-gcc PRIVATE ${UPROOT_GCC_LINK_FLAGS} "${UPROOT_ASAN_FLAGS}")
target_link_libraries(uproot-gcc PRIVATE Aspartame::Aspartame fmt::fmt spdlog::spdlog nlohmann_json::nlohmann_json libzstd_static bxzstr)

set_target_properties(uproot-gcc PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")




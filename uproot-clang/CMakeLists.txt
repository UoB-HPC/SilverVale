cmake_minimum_required(VERSION 3.19 FATAL_ERROR)
project(uproot-clang CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

#set(SV_CLANG_INCLUDE_PATHS "" CACHE STRING "LLVM+Clang include paths for Clang uproot clang plugin")


option(SV_ENABLE_UPROOT_CLANG_TESTS "Enable tests for the Clang uproot plugin" ON)
option(SV_USE_LIBCXX "Use libc++ instead of libstdc++" OFF)
set(CLANG_VARIANTS "system:/usr/include" CACHE STRING "A semicolon separated list of clang variants in the form of name:include_dir")

#set(CLANG_VARIANTS
#        "system:/usr/include"
#        "dpcpp:/home/tom/sycl_workspace/llvm/build/install/include"
#        "llvm:/home/tom/llvm_workspace/llvm-project/llvm/build/install/include"
#)

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
        "$<$<CONFIG:Debug>:${SV_LINK_DEBUG_FLAGS}>")

if (SV_USE_LIBCXX)
    list(APPEND UPROOT_CLANG_COMPILE_FLAGS -stdlib=libc++)
    list(APPEND UPROOT_CLANG_LINK_FLAGS -stdlib=libc++)
endif ()

set(UPROOT_ASAN_FLAGS
        "$<$<AND:$<CXX_COMPILER_ID:Clang>,$<CONFIG:Debug>>:-shared-libasan>")


add_custom_target(uproot-clang ALL)

foreach (variant_and_path ${CLANG_VARIANTS})
    string(REPLACE ":" ";" variant_and_path_list ${variant_and_path})
    list(GET variant_and_path_list 0 variant)
    list(GET variant_and_path_list 1 include_path)

    message(STATUS "Creating target variant: ${variant} with ${include_path}")

    if (NOT IS_DIRECTORY "${include_path}")
        message(FATAL_ERROR "The LLVM include path is not a valid directory.")
    elseif (NOT IS_DIRECTORY "${include_path}/clang")
        message(FATAL_ERROR "The 'clang' directory is missing, is the path a valid LLVM include directory?")
    elseif (NOT IS_DIRECTORY "${include_path}/llvm")
        message(FATAL_ERROR "The 'llvm' directory is missing, is the path a valid LLVM include directory?")
    endif ()


    add_library(uproot-clang-${variant} SHARED main.cpp compress.cpp tree_clangast.cpp tree_llvmir.cpp)
    set_target_properties(uproot-clang-${variant} PROPERTIES LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}")
    unset(targets)
    list(APPEND targets uproot-clang-${variant})
    add_dependencies(uproot-clang uproot-clang-${variant})


    if (SV_ENABLE_UPROOT_CLANG_TESTS)
        find_package(LLVM REQUIRED CONFIG)
        find_package(Clang REQUIRED CONFIG)
        message(STATUS "Found LLVM: ${LLVM_PACKAGE_VERSION}")
        message(STATUS "Found Clang: ${Clang_VERSION}")

        add_executable(uproot-clang-tests-${variant} test/ast.cpp)
        target_link_libraries(uproot-clang-tests-${variant} PRIVATE Catch2::Catch2WithMain LLVM clang-cpp uproot-clang-${variant})
        list(APPEND targets uproot-clang-tests-${variant})
    endif ()


    foreach (target ${targets})
        target_compile_options(${target} PRIVATE ${UPROOT_CLANG_COMPILE_FLAGS} -fno-rtti "${UPROOT_ASAN_FLAGS}")
        target_link_options(${target} PRIVATE ${UPROOT_CLANG_LINK_FLAGS} "${UPROOT_ASAN_FLAGS}")
        target_link_libraries(${target} PRIVATE BS_thread_pool Aspartame::Aspartame fmt::fmt-header-only spdlog::spdlog_header_only nlohmann_json::nlohmann_json libzstd_static bxzstr)
        target_include_directories(${target} PRIVATE BEFORE ${include_path} ${CMAKE_SOURCE_DIR}/include)
    endforeach ()


endforeach ()



include(ProcessorCount)

set(build_dir ${CMAKE_CURRENT_SOURCE_DIR}/headers-clang)
list(APPEND llvm_versions
        19.1.0-rc2
        18.1.8
        17.0.6
        16.0.6
        15.0.7
        14.0.6
#                13.0.1
#                12.0.1
#                11.1.0
#                10.0.1
)


foreach (version IN LISTS llvm_versions)


    string(REGEX MATCH "^([0-9]+)\\." major_version_match ${version})
    set(major_version ${CMAKE_MATCH_1})

    set(archive "${build_dir}/llvm-project-${version}.src.tar.xz")
    set(source "https://github.com/llvm/llvm-project/releases/download/llvmorg-${version}/llvm-project-${version}.src.tar.xz")
    if (NOT EXISTS "${archive}")
        message(STATUS "Downloading LLVM ${major_version}: ${source} -> ${archive}")
        file(DOWNLOAD
                "${source}"
                "${archive}"
                SHOW_PROGRESS
        )
        file(ARCHIVE_EXTRACT INPUT "${archive}" DESTINATION "${build_dir}")
    else ()
        message(STATUS "Using cached LLVM ${major_version} archive: ${archive}")
    endif ()

    execute_process(
            COMMAND ${CMAKE_COMMAND}
            -S "${build_dir}/llvm-project-${version}.src/llvm"
            -B "${build_dir}/build/llvm-${major_version}-build"

            -DCMAKE_BUILD_TYPE=Release
            -DBUILD_SHARED_LIBS=OFF
            -DLLVM_ENABLE_PROJECTS=clang
            -DLLVM_INCLUDE_BENCHMARKS=OFF
            -DLLVM_INCLUDE_TESTS=OFF
            -DLLVM_INCLUDE_DOCS=OFF
            -DLLVM_INCLUDE_EXAMPLES=OFF
            -DLLVM_BUILD_TESTS=OFF
            -DLLVM_BUILD_DOCS=OFF
            -DLLVM_BUILD_EXAMPLES=OFF
            "-DLLVM_TARGETS_TO_BUILD=X86\;AArch64" # not really important, just include the host platforms
            "-DCMAKE_INSTALL_PREFIX=${build_dir}/llvm-${major_version}"

            WORKING_DIRECTORY ${build_dir}
            RESULT_VARIABLE result)

    if (NOT result EQUAL "0")
        message(FATAL_ERROR "LLVM ${major_version} configure did not succeed")
    else ()
        message(STATUS "LLVM ${major_version} configuration complete, starting include targets")
    endif ()

    ProcessorCount(nproc)

    execute_process(
            COMMAND ${CMAKE_COMMAND}
            --build "${build_dir}/build/llvm-${major_version}-build"
            --target install-llvm-headers install-clang-headers
            -- -j ${nproc}
            WORKING_DIRECTORY ${build_dir}
            RESULT_VARIABLE result)

    if (NOT result EQUAL "0")
        message(FATAL_ERROR "LLVM ${major_version} header build did not succeed")
    else ()
        message(STATUS "LLVM ${major_version} header complete")
    endif ()

endforeach ()
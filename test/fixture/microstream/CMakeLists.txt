cmake_minimum_required(VERSION 3.18)
project(microstream CXX)


set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)

if (USE_CUDA)
    set(CMAKE_CUDA_FLAGS "${CMAKE_CUDA_FLAGS} -std=c++17")
    enable_language(CUDA)
endif ()


add_executable(microstream main.cpp)


if (USE_SERIAL)
    target_compile_definitions(microstream PUBLIC USE_SERIAL)
elseif (USE_OMP)
    find_package(OpenMP)
    target_link_libraries(microstream PUBLIC OpenMP::OpenMP_CXX)
    target_compile_definitions(microstream PUBLIC USE_OMP)
elseif (USE_OMP_TARGET)
    find_package(OpenMP)
    target_link_libraries(microstream PUBLIC OpenMP::OpenMP_CXX)
    target_compile_definitions(microstream PUBLIC USE_OMP_TARGET)
    target_compile_options(microstream PUBLIC --offload-arch=${USE_OMP_TARGET})
    target_link_options(microstream PUBLIC --offload-arch=${USE_OMP_TARGET} ${OpenMP_CXX_FLAGS})
elseif (USE_CUDA)
    target_compile_definitions(microstream PUBLIC USE_CUDA)
    set_source_files_properties(main.cpp PROPERTIES LANGUAGE CUDA)
    target_compile_options(microstream PUBLIC -std=c++17 --offload-arch=${USE_CUDA} -xcuda)
    target_link_options(microstream PUBLIC -std=c++17 --offload-arch=${USE_CUDA})
elseif (USE_HIP)
    target_compile_definitions(microstream PUBLIC USE_HIP)
    target_compile_options(microstream PUBLIC -std=c++17 --offload-arch=${USE_HIP} -xhip)
    target_link_options(microstream PUBLIC -std=c++17 --offload-arch=${USE_HIP})
endif ()


target_compile_options(microstream PUBLIC "$<$<CONFIG:Release>:-Ofast;-march=native>")
target_compile_options(microstream PUBLIC "$<$<CONFIG:Debug>:-fsanitize=address;-fsanitize=undefined;-fno-omit-frame-pointer>")
target_link_options(microstream PUBLIC "$<$<CONFIG:Debug>:-fsanitize=address;-fsanitize=undefined;-fno-omit-frame-pointer>")

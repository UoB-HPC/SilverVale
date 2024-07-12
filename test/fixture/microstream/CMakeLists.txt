cmake_minimum_required(VERSION 3.28)
project(microstream CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(microstream main.cpp)

target_compile_options(microstream PUBLIC "$<$<CONFIG:Release>:-Ofast;-march=native>")
target_compile_options(microstream PUBLIC "$<$<CONFIG:Debug>:-fsanitize=address;-fsanitize=undefined;-fno-omit-frame-pointer>")
target_link_options(microstream PUBLIC "$<$<CONFIG:Debug>:-fsanitize=address;-fsanitize=undefined;-fno-omit-frame-pointer>")

if (USE_OMP)
    find_package(OpenMP)
    target_link_libraries(microstream PUBLIC OpenMP::OpenMP_CXX)
    target_compile_definitions(microstream PUBLIC USE_OMP)
endif ()

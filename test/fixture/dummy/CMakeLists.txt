cmake_minimum_required(VERSION 3.28)
project(dummy CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(dummy main.cpp second.cpp)

target_compile_options(dummy PUBLIC "$<$<CONFIG:Release>:-Ofast;-march=native>")
target_compile_options(dummy PUBLIC "$<$<CONFIG:Debug>:-fsanitize=address;-fsanitize=undefined;-fno-omit-frame-pointer>")
target_link_options(dummy PUBLIC "$<$<CONFIG:Debug>:-fsanitize=address;-fsanitize=undefined;-fno-omit-frame-pointer>")

if (USE_OMP)
    find_package(OpenMP)
    target_link_libraries(dummy PUBLIC OpenMP::OpenMP_CXX)
    target_compile_definitions(dummy PUBLIC USE_OMP)
endif ()

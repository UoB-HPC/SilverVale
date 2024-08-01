cmake_minimum_required(VERSION 3.18)
project(simple CXX Fortran)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_EXTENSIONS OFF)


if (USE_FORTRAN)
    add_executable(simple main.F90 second.F90)
    set(LANG "Fortran")
else ()
    add_executable(simple main.cpp second.cpp)
    set(LANG "CXX")
endif ()

target_compile_options(simple PUBLIC "$<$<CONFIG:Release>:-Ofast;-march=native>")
target_compile_options(simple PUBLIC "$<$<CONFIG:Debug>:-fsanitize=address;-fsanitize=undefined;-fno-omit-frame-pointer>")
target_link_options(simple PUBLIC "$<$<CONFIG:Debug>:-fsanitize=address;-fsanitize=undefined;-fno-omit-frame-pointer>")

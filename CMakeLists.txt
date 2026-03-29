cmake_minimum_required(VERSION 3.20)
project(default_calc)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(default_calc/include)

add_executable(default_calc
        default_calc/src/main.cpp
        default_calc/src/calc.cpp
)

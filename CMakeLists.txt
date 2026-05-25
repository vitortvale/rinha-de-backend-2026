cmake_minimum_required(VERSION 3.25)

project(rinha_2026_cpp LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_compile_options(
  -O3
  -march=x86-64-v3
  -fno-exceptions
  -fno-rtti
  -Wall
  -Wextra
  -Wpedantic
)

add_library(rinha_common INTERFACE)
target_include_directories(rinha_common INTERFACE cpp/src)

add_executable(rinha_api cpp/src/api.cpp)
target_link_libraries(rinha_api PRIVATE rinha_common)

add_executable(rinha_healthcheck cpp/src/healthcheck.cpp)
target_link_libraries(rinha_healthcheck PRIVATE rinha_common)

add_executable(rinha_lb cpp/src/lb.cpp)
target_link_libraries(rinha_lb PRIVATE rinha_common)

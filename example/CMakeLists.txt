cmake_minimum_required(VERSION 3.19)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

project ("test_example")

include_directories("${CMAKE_CURRENT_SOURCE_DIR}/../header/")

add_executable(TestApp "source.cpp")

install(TARGETS TestApp RUNTIME DESTINATION ${CMAKE_CURRENT_BINARY_DIR}/../bin/)

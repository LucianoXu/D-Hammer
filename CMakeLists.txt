cmake_minimum_required(VERSION 3.28)
project(UALGProject LANGUAGES CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

#######################################################
# Include FetchContent module

include(FetchContent)

# Download GoogleTest
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/refs/tags/release-1.12.1.zip  # Use the latest stable release
  DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)
# Prevent GoogleTest from installing
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

# Use find_package to locate Boost
find_package(Boost)

# check if Boost was found
if(Boost_FOUND)
    message(STATUS "Boost found at " ${Boost_INCLUDE_DIRS})
    # output boost version
    message(STATUS "Boost version: " ${Boost_VERSION})
    include_directories(${Boost_INCLUDE_DIRS})
else()
    message(FATAL_ERROR "Boost not found. Please install Boost library for compilation.")
endif()

# set up ANTLR
set(ANTLR4 "${CMAKE_CURRENT_SOURCE_DIR}/extern/antlr-4.13.2-complete.jar")
message(STATUS "ANTLR4 executable: ${ANTLR4}")


####################################################
# Compiler Setting

# Set C++ standard
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

# Define build types with a default
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Choose the type of build: Debug or Release" FORCE)
endif()

# Set the compiler
set(CMAKE_CXX_COMPILER "clang++")


# Set build flags
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -O0 -g")
elseif(CMAKE_BUILD_TYPE STREQUAL "Release")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -O3 -DNDEBUG")
elseif(CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -O2 -g")
else()
    message(FATAL_ERROR "Unknown build type: ${CMAKE_BUILD_TYPE}")
endif()


# Enable testing
enable_testing()

####################################################
# C++

add_subdirectory(cpp)

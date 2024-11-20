cmake_minimum_required(VERSION 3.10)
project(UALGProject LANGUAGES CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

#######################################################
# Include FetchContent module

include(FetchContent)

# Download GoogleTest
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/refs/tags/release-1.12.1.zip  # Use the latest stable release
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
# DEPENDENCIES

add_subdirectory(third-party)

####################################################

add_subdirectory(src)

####################################################
# The main executable

add_executable(MAIN main.cpp)

target_link_libraries(
    MAIN 
    PUBLIC 
        DEPENDENCIES 
        UALG
        SCALAR
        SCALAR_VEC
)

####################################################

# Add the "run" target to execute the built executable
add_custom_target(run
    COMMAND ./MAIN
    DEPENDS MAIN
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Running the MAIN executable"
)

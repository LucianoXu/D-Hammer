
# ANTLR
file(GLOB_RECURSE 
    ANTLR_SRCS 
    "${CMAKE_CURRENT_SOURCE_DIR}/antlr_runtime/*.cpp")

add_library(
    DEPENDENCIES
    ${ANTLR_SRCS}
)

target_include_directories(
    DEPENDENCIES 
    PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/antlr_runtime)

# Boost
target_link_libraries(
    DEPENDENCIES
    PUBLIC
    Boost::boost
)
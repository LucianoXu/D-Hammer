
add_library(
    UALG 

    term.cpp
    termbank.cpp
    ualgparser.cpp
    rewrite.cpp
)

target_link_libraries(
    UALG
    PUBLIC
        ASTPARSER
        DEPENDENCIES
)

target_include_directories(
    UALG
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)
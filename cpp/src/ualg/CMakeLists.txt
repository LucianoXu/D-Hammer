
add_library(
    UALG 

    AC_by_vec.cpp
    ualgparser.cpp
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
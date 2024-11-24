
add_library(
    DIRACOQ

    reserved.cpp
    calculus.cpp
    prover.cpp
)

target_link_libraries(
    DIRACOQ
    PUBLIC
        UALG
        ASTPARSER
        DEPENDENCIES
)

target_include_directories(
    DIRACOQ
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

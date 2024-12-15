
add_library(
    DIRACOQ

    symbols.cpp
    calculus.cpp
    # reduction.cpp
    # trace2coq.cpp
    # prover.cpp
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

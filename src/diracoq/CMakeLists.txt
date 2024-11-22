add_library(
    DIRACOQ

    calculus.cpp
)

target_link_libraries(
    DIRACOQ
    PUBLIC
        UALG
        DEPENDENCIES
)

target_include_directories(
    DIRACOQ
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

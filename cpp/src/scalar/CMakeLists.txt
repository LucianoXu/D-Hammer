add_library(
    SCALAR

    scalar.cpp
)

target_link_libraries(
    SCALAR
    PUBLIC
        UALG
        DEPENDENCIES
)

target_include_directories(
    SCALAR
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)


#################################
# Tests list
set(tests
    test_scalar
)

foreach(test ${tests})
    add_executable(${test} ${test}.cpp)

    target_link_libraries(
        ${test}
        gtest_main SCALAR
    )

    add_test(
        NAME ${test}
        COMMAND ${test}
    )
endforeach()

#################################
# Tests list
#
# Including all examples for the tool

add_library(
    EXAMPLES

    examples.cpp
)

target_link_libraries(
    EXAMPLES
    PUBLIC
        DHAMMER
)


target_include_directories(
    EXAMPLES
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

#############################################
# Test list

set(tests
    test_examples
    test_timing
)

foreach(test ${tests})
    add_executable(${test} ${test}.cpp)

    target_link_libraries(
        ${test}
        gtest_main DHAMMER EXAMPLES
    )

    add_test(
        NAME ${test}
        COMMAND ${test}
    )
endforeach()

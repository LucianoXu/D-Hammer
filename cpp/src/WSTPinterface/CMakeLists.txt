#################################
# Tests list
#
# Including all examples for the tool

add_library(
    WSTPINTERFACE
    
    WSTPinterface.cpp
)

target_link_libraries(
    WSTPINTERFACE
    PUBLIC
        ASTPARSER
)


target_include_directories(
    WSTPINTERFACE
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)

#############################################
# Test list

set(tests
    test_WSTPinterface
)

foreach(test ${tests})
    add_executable(${test} ${test}.cpp)

    target_link_libraries(
        ${test}
        gtest_main WSTPINTERFACE
    )

    add_test(
        NAME ${test}
        COMMAND ${test}
    )
endforeach()

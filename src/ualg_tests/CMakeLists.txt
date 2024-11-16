#################################
# Tests list
set(tests
    test_ualg_term
    test_ualg_parsing
    test_ualg_rewrite
)

foreach(test ${tests})
    add_executable(${test} ${test}.cpp)

    target_link_libraries(
        ${test}
        gtest_main UALG
    )

    add_test(
        NAME ${test}
        COMMAND ${test}
    )
endforeach()

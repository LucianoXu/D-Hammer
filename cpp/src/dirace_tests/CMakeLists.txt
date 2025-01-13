#################################
# Tests list
set(tests
    test_parser
    test_syntax_theory
    test_calculus
    test_reduction
    test_special_eq
    test_prover
)

foreach(test ${tests})
    add_executable(${test} ${test}.cpp)

    target_link_libraries(
        ${test}
        gtest_main DIRACE
    )

    add_test(
        NAME ${test}
        COMMAND ${test}
    )
endforeach()

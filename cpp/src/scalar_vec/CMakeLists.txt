add_library(
    SCALAR_VEC

    scalar_vec.cpp
)

target_link_libraries(
    SCALAR_VEC
    PUBLIC
        UALG
        DEPENDENCIES
)

target_include_directories(
    SCALAR_VEC
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
)


#################################
# Tests list
set(tests
    test_scalar_vec
)

foreach(test ${tests})
    add_executable(${test} ${test}.cpp)

    target_link_libraries(
        ${test}
        gtest_main SCALAR_VEC
    )

    add_test(
        NAME ${test}
        COMMAND ${test}
    )
endforeach()

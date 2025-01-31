
set(ANTLR_AST_DIR "${CMAKE_CURRENT_SOURCE_DIR}/antlr_generated")

set(ANTLR_AST_GEN_FILES 
    ${ANTLR_AST_DIR}/AST.interp 
    ${ANTLR_AST_DIR}/AST.tokens 
    ${ANTLR_AST_DIR}/ASTBaseListener.cpp 
    ${ANTLR_AST_DIR}/ASTBaseListener.h 
    ${ANTLR_AST_DIR}/ASTBaseVisitor.cpp 
    ${ANTLR_AST_DIR}/ASTBaseVisitor.h 
    ${ANTLR_AST_DIR}/ASTLexer.cpp 
    ${ANTLR_AST_DIR}/ASTLexer.h 
    ${ANTLR_AST_DIR}/ASTLexer.interp 
    ${ANTLR_AST_DIR}/ASTLexer.tokens 
    ${ANTLR_AST_DIR}/ASTListener.cpp 
    ${ANTLR_AST_DIR}/ASTListener.h 
    ${ANTLR_AST_DIR}/ASTParser.cpp 
    ${ANTLR_AST_DIR}/ASTParser.h 
    ${ANTLR_AST_DIR}/ASTVisitor.cpp 
    ${ANTLR_AST_DIR}/ASTVisitor.h
)


# Custom command for ANTLR
add_custom_command(
    OUTPUT ${ANTLR_AST_GEN_FILES}
    COMMAND java -jar /Users/xuyingte/Java3P/antlr-4.13.2-complete.jar -Dlanguage=Cpp -visitor ${CMAKE_CURRENT_SOURCE_DIR}/AST.g4 -o ${ANTLR_AST_DIR}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/AST.g4
    COMMENT "Running ANTLR to generate C++ code from AST.g4"
)

add_custom_target(antlr_ast_gen
    DEPENDS ${ANTLR_AST_GEN_FILES}
)

add_library(
    ASTPARSER

    astparser.cpp

    ${ANTLR_AST_GEN_FILES}
)

target_link_libraries(
    ASTPARSER
    PUBLIC
        DEPENDENCIES
)

target_include_directories(
    ASTPARSER
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${ANTLR_AST_DIR}
)

add_dependencies(ASTPARSER antlr_ast_gen)


#################################
# Test list
set(tests
    test_astparser
)

foreach(test ${tests})
    add_executable(${test} ${test}.cpp)

    target_link_libraries(
        ${test}
        gtest_main ASTPARSER
    )

    add_test(
        NAME ${test}
        COMMAND ${test}
    )
endforeach()

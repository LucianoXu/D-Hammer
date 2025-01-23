

set(ANTLR_DIRACE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/antlr_generated")

set(ANTLR_DIRACE_GEN_FILES 
    ${ANTLR_DIRACE_DIR}/DIRACE.interp 
    ${ANTLR_DIRACE_DIR}/DIRACE.tokens 
    ${ANTLR_DIRACE_DIR}/DIRACEBaseListener.cpp 
    ${ANTLR_DIRACE_DIR}/DIRACEBaseListener.h 
    ${ANTLR_DIRACE_DIR}/DIRACEBaseVisitor.cpp 
    ${ANTLR_DIRACE_DIR}/DIRACEBaseVisitor.h 
    ${ANTLR_DIRACE_DIR}/DIRACELexer.cpp 
    ${ANTLR_DIRACE_DIR}/DIRACELexer.h 
    ${ANTLR_DIRACE_DIR}/DIRACELexer.interp 
    ${ANTLR_DIRACE_DIR}/DIRACELexer.tokens 
    ${ANTLR_DIRACE_DIR}/DIRACEListener.cpp 
    ${ANTLR_DIRACE_DIR}/DIRACEListener.h 
    ${ANTLR_DIRACE_DIR}/DIRACEParser.cpp 
    ${ANTLR_DIRACE_DIR}/DIRACEParser.h 
    ${ANTLR_DIRACE_DIR}/DIRACEVisitor.cpp 
    ${ANTLR_DIRACE_DIR}/DIRACEVisitor.h
)


# Custom command for ANTLR
add_custom_command(
    OUTPUT ${ANTLR_DIRACE_GEN_FILES}
    COMMAND java -jar /Users/xuyingte/Java3P/antlr-4.13.2-complete.jar -Dlanguage=Cpp -visitor ${CMAKE_CURRENT_SOURCE_DIR}/DIRACE.g4 -o ${ANTLR_DIRACE_DIR}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/DIRACE.g4  # Declare DIRACE.g4 as a dependency
    COMMENT "Running ANTLR to generate C++ code from DIRACE.g4"
)

add_custom_target(antlr_dirace_gen
    DEPENDS ${ANTLR_DIRACE_GEN_FILES}
)

add_library(
    DIRACE

    symbols.cpp
    dirace_parser.cpp
    syntax_theory.cpp
    calculus.cpp
    reduction.cpp
    trace.cpp
    prover.cpp

    ${ANTLR_DIRACE_GEN_FILES}
)

target_link_libraries(
    DIRACE
    PUBLIC
        UALG
        ASTPARSER
        WSTPINTERFACE
        DEPENDENCIES
)

target_include_directories(
    DIRACE
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${ANTLR_DIRACE_DIR}
)

add_dependencies(DIRACE antlr_dirace_gen)

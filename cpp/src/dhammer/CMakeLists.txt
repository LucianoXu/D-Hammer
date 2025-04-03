

set(ANTLR_DHAMMER_DIR "${CMAKE_CURRENT_SOURCE_DIR}/antlr_generated")

set(ANTLR_DHAMMER_GEN_FILES 
    ${ANTLR_DHAMMER_DIR}/DHAMMER.interp 
    ${ANTLR_DHAMMER_DIR}/DHAMMER.tokens 
    ${ANTLR_DHAMMER_DIR}/DHAMMERBaseListener.cpp 
    ${ANTLR_DHAMMER_DIR}/DHAMMERBaseListener.h 
    ${ANTLR_DHAMMER_DIR}/DHAMMERBaseVisitor.cpp 
    ${ANTLR_DHAMMER_DIR}/DHAMMERBaseVisitor.h 
    ${ANTLR_DHAMMER_DIR}/DHAMMERLexer.cpp 
    ${ANTLR_DHAMMER_DIR}/DHAMMERLexer.h 
    ${ANTLR_DHAMMER_DIR}/DHAMMERLexer.interp 
    ${ANTLR_DHAMMER_DIR}/DHAMMERLexer.tokens 
    ${ANTLR_DHAMMER_DIR}/DHAMMERListener.cpp 
    ${ANTLR_DHAMMER_DIR}/DHAMMERListener.h 
    ${ANTLR_DHAMMER_DIR}/DHAMMERParser.cpp 
    ${ANTLR_DHAMMER_DIR}/DHAMMERParser.h 
    ${ANTLR_DHAMMER_DIR}/DHAMMERVisitor.cpp 
    ${ANTLR_DHAMMER_DIR}/DHAMMERVisitor.h
)


# Custom command for ANTLR
add_custom_command(
    OUTPUT ${ANTLR_DHAMMER_GEN_FILES}
    COMMAND java -jar ANTLR -Dlanguage=Cpp -visitor ${CMAKE_CURRENT_SOURCE_DIR}/DHAMMER.g4 -o ${ANTLR_DHAMMER_DIR}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/DHAMMER.g4  # Declare DHAMMER.g4 as a dependency
    COMMENT "Running ANTLR to generate C++ code from DHAMMER.g4"
)

add_custom_target(antlr_dhammer_gen
    DEPENDS ${ANTLR_DHAMMER_GEN_FILES}
)

add_library(
    DHAMMER

    symbols.cpp
    dhammer_parser.cpp
    syntax_theory.cpp
    calculus.cpp
    reduction.cpp
    trace.cpp
    prover.cpp

    ${ANTLR_DHAMMER_GEN_FILES}
)

target_link_libraries(
    DHAMMER
    PUBLIC
        UALG
        ASTPARSER
        WSTPINTERFACE
        DEPENDENCIES
)

target_include_directories(
    DHAMMER
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${ANTLR_DHAMMER_DIR}
)

add_dependencies(DHAMMER antlr_dhammer_gen)

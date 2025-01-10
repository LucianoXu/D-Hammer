

set(ANTLR_DIRACOQ_DIR "${CMAKE_CURRENT_SOURCE_DIR}/antlr_generated")

set(ANTLR_DIRACOQ_GEN_FILES 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQ.interp 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQ.tokens 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQBaseListener.cpp 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQBaseListener.h 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQBaseVisitor.cpp 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQBaseVisitor.h 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQLexer.cpp 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQLexer.h 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQLexer.interp 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQLexer.tokens 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQListener.cpp 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQListener.h 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQParser.cpp 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQParser.h 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQVisitor.cpp 
    ${ANTLR_DIRACOQ_DIR}/DIRACOQVisitor.h
)


# Custom command for ANTLR
add_custom_command(
    OUTPUT ${ANTLR_DIRACOQ_GEN_FILES}
    COMMAND java -jar /Users/xuyingte/Java3P/antlr-4.13.2-complete.jar -Dlanguage=Cpp -visitor ${CMAKE_CURRENT_SOURCE_DIR}/DIRACOQ.g4 -o ${ANTLR_DIRACOQ_DIR}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/DIRACOQ.g4  # Declare DIRACOQ.g4 as a dependency
    COMMENT "Running ANTLR to generate C++ code from DIRACOQ.g4"
)

add_custom_target(antlr_diracoq_gen
    DEPENDS ${ANTLR_DIRACOQ_GEN_FILES}
)

add_library(
    DIRACOQ

    symbols.cpp
    diracoq_parser.cpp
    syntax_theory.cpp
    calculus.cpp
    reduction.cpp
    label_elim.cpp
    trace.cpp
    prover.cpp

    ${ANTLR_DIRACOQ_GEN_FILES}
)

target_link_libraries(
    DIRACOQ
    PUBLIC
        UALG
        ASTPARSER
        WSTPINTERFACE
        DEPENDENCIES
)

target_include_directories(
    DIRACOQ
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${ANTLR_DIRACOQ_DIR}
)

add_dependencies(DIRACOQ antlr_diracoq_gen)

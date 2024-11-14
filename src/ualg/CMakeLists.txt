
set(ANTLR_UALG_DIR "${CMAKE_CURRENT_SOURCE_DIR}/antlr_generated")

set(ANTLR_UALG_GEN_FILES 
    ${ANTLR_UALG_DIR}/UALG.interp 
    ${ANTLR_UALG_DIR}/UALG.tokens 
    ${ANTLR_UALG_DIR}/UALGBaseListener.cpp 
    ${ANTLR_UALG_DIR}/UALGBaseListener.h 
    ${ANTLR_UALG_DIR}/UALGBaseVisitor.cpp 
    ${ANTLR_UALG_DIR}/UALGBaseVisitor.h 
    ${ANTLR_UALG_DIR}/UALGLexer.cpp 
    ${ANTLR_UALG_DIR}/UALGLexer.h 
    ${ANTLR_UALG_DIR}/UALGLexer.interp 
    ${ANTLR_UALG_DIR}/UALGLexer.tokens 
    ${ANTLR_UALG_DIR}/UALGListener.cpp 
    ${ANTLR_UALG_DIR}/UALGListener.h 
    ${ANTLR_UALG_DIR}/UALGParser.cpp 
    ${ANTLR_UALG_DIR}/UALGParser.h 
    ${ANTLR_UALG_DIR}/UALGVisitor.cpp 
    ${ANTLR_UALG_DIR}/UALGVisitor.h
)


# Custom command for ANTLR
add_custom_command(
    OUTPUT ${ANTLR_UALG_GEN_FILES}
    COMMAND java -jar /Users/xuyingte/Java3P/antlr-4.13.2-complete.jar -Dlanguage=Cpp -visitor ${CMAKE_CURRENT_SOURCE_DIR}/UALG.g4 -o ${ANTLR_UALG_DIR}
    COMMENT "Running ANTLR to generate C++ code from UALG.g4"
)

add_custom_target(antlr_ualg_gen
    DEPENDS ${ANTLR_UALG_GEN_FILES}
)

add_library(
    UALG 

    term.cpp
    termbank.cpp
    ualgparser.cpp
    rewrite.cpp

    ${ANTLR_UALG_GEN_FILES}
)

target_link_libraries(
    UALG
    PUBLIC
        DEPENDENCIES
)

target_include_directories(
    UALG
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${ANTLR_UALG_DIR}
)

add_dependencies(UALG antlr_ualg_gen)
#pragma once

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <vector>

#include "wstp.h"

#include "astparser.hpp"


namespace wstp {

    extern const int MACOS_ARGC;
    extern char** const MACOS_ARGV;

    extern std::vector<std::pair<WSENV, WSLINK>> links;

    std::pair<WSENV, WSLINK> init_and_openlink(int argc, char* argv[]);

    /**
     * @brief Transform and push the AST to the WSTP link.
     * 
     * @param lp 
     * @param ast 
     */
    void ast_to_WS(WSLINK lp, const astparser::AST& ast);

    /**
     * @brief Read the WSTP link and transform it into an AST.
     * 
     * @param lp 
     * @return astparser::AST 
     */
    astparser::AST WS_to_ast(WSLINK lp);

} // namespace wstp
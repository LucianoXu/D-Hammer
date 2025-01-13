
#pragma once

#include "astparser.hpp"
#include "antlr4-runtime.h"

#include "DIRACELexer.h"
#include "DIRACEParser.h"
#include "DIRACEBaseListener.h"

#include <stack>

namespace dirace {

    /**
     * @brief Parse the given code into an AST.
     * 
     * @param code 
     * @return AST 
     */
    std::optional<astparser::AST> parse(const std::string& code, bool debug = false);

} // namespace ast
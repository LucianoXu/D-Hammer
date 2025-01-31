
#pragma once

#include "astparser.hpp"
#include "antlr4-runtime.h"

#include "DHAMMERLexer.h"
#include "DHAMMERParser.h"
#include "DHAMMERBaseListener.h"

#include <stack>

namespace dhammer {

    /**
     * @brief Parse the given code into an AST.
     * 
     * @param code 
     * @return AST 
     */
    std::optional<astparser::AST> parse(const std::string& code, bool debug = false);

} // namespace dhammer
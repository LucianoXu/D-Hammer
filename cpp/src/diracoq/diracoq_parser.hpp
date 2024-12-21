
#pragma once

#include "astparser.hpp"
#include "antlr4-runtime.h"

#include "DIRACOQLexer.h"
#include "DIRACOQParser.h"
#include "DIRACOQBaseListener.h"

#include <stack>

namespace diracoq {

    /**
     * @brief Parse the given code into an AST.
     * 
     * @param code 
     * @return AST 
     */
    std::optional<astparser::AST> parse(const std::string& code, bool debug = false);

} // namespace ast
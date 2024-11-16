/*

The simple AST parser module.

See AST.g4 for the grammar definition.

*/

#pragma once

#include "antlr4-runtime.h"

#include "ASTLexer.h"
#include "ASTParser.h"
#include "ASTBaseListener.h"

#include <stack>

namespace astparser {

    struct AST {
        std::string head;
        std::vector<AST> children;

        bool operator == (const AST& other) const {
            if (head != other.head) {
                return false;
            }

            if (children.size() != other.children.size()) {
                return false;
            }

            for (size_t i = 0; i < children.size(); ++i) {
                if (children[i] != other.children[i]) {
                    return false;
                }
            }

            return true;
        }
    };

    /**
     * @brief Parse the given code into an AST.
     * 
     * @param code 
     * @return AST 
     */
    AST parse(const std::string& code);

} // namespace ast
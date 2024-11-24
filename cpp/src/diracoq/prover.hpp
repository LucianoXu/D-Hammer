// The function to process the command ast and operate the kernel

#pragma once

#include <iostream>
#include "calculus.hpp"

namespace diracoq {
    
    class Prover {
    protected:
        Kernel kernel;
        std::ostream& output;

    protected:
        bool check_id(const astparser::AST& ast) {
            if (ast.children.size() != 0) {
                output << "Error: the identifier is not valid." << std::endl;
                output << "In the command: " << ast.to_string() << std::endl;
                return false;
            }
            return true;
        }

    public:
        Prover(std::ostream& _output = std::cout) : output(_output) {}

        inline bool process(const std::string& code) {
            auto ast = astparser::parse(code);
            if (ast.has_value()) {
                return process(ast.value());
            }
            else{
                output << "Error: the code is not valid." << std::endl;
                return false;
            }
        }

        bool process(const astparser::AST& ast);
    };

} // namespace diracoq
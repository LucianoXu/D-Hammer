// The function to process the command ast and operate the kernel

#pragma once

#include <iostream>
#include <fstream>

#include "calculus.hpp"
#include "trace.hpp"

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
        Kernel& get_kernel() { return kernel; }
        
        Prover(WSLINK wstp_link = nullptr, std::ostream& _output = std::cout) : kernel(wstp_link), output(_output) {}

        // copy constructor (coq_file is not copied)
        Prover(const Prover& other) : kernel(other.kernel), output(other.output) {}


        ~Prover() {}

        inline bool process(const std::string& code) {
            auto ast = parse(code);
            if (ast.has_value()) {
                return process(ast.value());
            }
            else{
                output << "Error: the code is not valid." << std::endl;
                return false;
            }
        }

        bool process(const astparser::AST& ast);

        inline bool check_eq(const std::string& codeA, const std::string& codeB) {
            auto astA = parse(codeA);
            auto astB = parse(codeB);
            if (astA.has_value() and astB.has_value()) {
                return check_eq(astA.value(), astB.value());
            }
            else{
                output << "Error: the code is not valid." << std::endl;
                return false;
            }
        }
        
        /**
         * @brief Checks whether the two terms are equal. Return the result as a boolean.
         * 
         * @param codeA 
         * @param codeB 
         * @return true 
         * @return false 
         */
        bool check_eq(const astparser::AST& codeA, const astparser::AST& codeB);
    };

    /**
     * @brief Return the prover with standard definitions.
     * 
     * @return Prover 
     */
    Prover std_prover(WSLINK wstp_link = nullptr);

} // namespace diracoq
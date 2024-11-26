#include "diracoq.hpp"

namespace diracoq {
    using namespace ualg;

    bool Prover::process(const astparser::AST& ast) {
        // Group ( ... )
        try {
            if (ast.head == "Group") {
                for (const auto& cmd : ast.children) {
                    process(cmd);
                }
                return true;
            }
            else if (ast.head == "Def") {
                // Def(x t)
                if (ast.children.size() == 2) {
                    if (!check_id(ast.children[0])) return false;

                    auto name = ast.children[0].head;
                    auto term = kernel.parse(ast.children[1]);
                    kernel.def(kernel.register_symbol(name), term);
                    return true;
                }
                // Def(x t T)
                if (ast.children.size() == 3) {
                    if (!check_id(ast.children[0])) return false;

                    auto name = ast.children[0].head;
                    auto term = kernel.parse(ast.children[1]);
                    auto type = kernel.parse(ast.children[2]);
                    kernel.def(kernel.register_symbol(name), term, type);
                    return true;
                }
                else {
                    output << "Error: the definition is not valid." << std::endl;
                    output << "In the command: " << ast.to_string() << std::endl;
                    return false;
                }
            }
            else if (ast.head == "Var")  {
                // Assum(x T)
                if (ast.children.size() == 2) {
                    if (!check_id(ast.children[0])) return false;

                    
                    auto name = ast.children[0].head;
                    auto type = kernel.parse(ast.children[1]);
                    kernel.assum(kernel.register_symbol(name), type);
                    return true;
                }
                else {
                    output << "Error: the assumption is not valid." << std::endl;
                    output << "In the command: " << ast.to_string() << std::endl;
                    return false;
                }
            }
            else if (ast.head == "Check") {
                // Check(x)
                if (ast.children.size() == 1) {
                    try {
                        auto term = kernel.parse(ast.children[0]);
                        auto type = kernel.calc_type(term);
                        output << kernel.term_to_string(term) << " : " << kernel.term_to_string(type) << std::endl;
                        return true;
                    }
                    catch (const std::exception& e) {
                        output << "Error: " << e.what() << std::endl;
                        return false;
                    }
                }
            }
            // Show(x)
            else if (ast.head == "Show") {
                if (ast.children.size() == 1) {
                    if (!check_id(ast.children[0])) return false;

                    auto name = ast.children[0].head;
                    try {
                        // get the definition in the env
                        auto find_def = kernel.find_in_env(kernel.register_symbol(name));
                        if (find_def == std::nullopt) {
                            output << "Error: the symbol '" << name << "' is not defined." << std::endl;
                            return false;
                        }
                        output << kernel.dec_to_string(name, find_def.value()) << std::endl;
                        return true;
                    }
                    catch (const std::exception& e) {
                        output << "Error: " << e.what() << std::endl;
                        return false;
                    }
                }
            }
            // ShowAll
            else if (ast.head == "ShowAll") {
                if (ast.children.size() == 0) {
                    output << "Environment:" << std::endl;
                    output << kernel.env_to_string() << std::endl;
                    return true;
                }
                else {
                    output << "Error: the command is not valid." << std::endl;
                    output << "In the command: " << ast.to_string() << std::endl;
                    return false;
                }
            }
            else if (ast.head == "Normalize") {
                if (ast.children.size() != 1) {
                    output << "Error: Normalize command should have one argument." << std::endl;
                    return false;
                }
                // calculate the normalized term
                auto term = static_cast<const NormalTerm<int>*>(kernel.parse(ast.children[0]));
                auto normalized_term = pos_rewrite_repeated(kernel, term, rules);

                auto type = kernel.calc_type(normalized_term);
                
                // Output the normalized term
                output << kernel.term_to_string(normalized_term) + " : " + kernel.term_to_string(type)  << std::endl;
                return true;
            }
        }
        catch (const std::exception& e) {
            output << "Error: " << e.what() << std::endl;
            return false;
        }

        // bad command
        output << "Error: the command is not valid." << std::endl;
        output << "In the command: " << ast.to_string() << std::endl;
        return false;
    }

} // namespace diracoq
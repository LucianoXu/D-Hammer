#include "diracoq.hpp"

namespace diracoq {
    using namespace std;
    using namespace ualg;

    bool Prover::process(const astparser::AST& ast) {
        // Group ( ... )
        try {
            if (ast.head == "Group") {
                for (const auto& cmd : ast.children) {
                    if (!process(cmd)){
                        return false;
                    }
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

                    // generate Coq code
                    if (gen_coq) {
                        append_coq_code("(* " + ast.to_string() + " *)\n");
                        append_coq_code("Definition " + name + " := " + term_to_coq(kernel, term) + ".\n\n");
                    }

                    return true;
                }
                // Def(x t T)
                if (ast.children.size() == 3) {
                    if (!check_id(ast.children[0])) return false;

                    auto name = ast.children[0].head;
                    auto term = kernel.parse(ast.children[1]);
                    auto type = kernel.parse(ast.children[2]);
                    kernel.def(kernel.register_symbol(name), term, type);

                    // generate Coq code
                    if (gen_coq) {
                        append_coq_code("(* " + ast.to_string() + " *)\n");
                        append_coq_code("Definition " + name + " : " + term_to_coq(kernel, type) + " := " + term_to_coq(kernel, term) + ".\n\n");
                    }

                    return true;
                }
                else {
                    output << "Error: the definition is not valid." << endl;
                    output << "In the command: " << ast.to_string() << endl;
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

                    // generate Coq code
                    if (gen_coq) {
                        append_coq_code("(* " + ast.to_string() + " *)\n");
                        append_coq_code("Variable " + name + " : " + term_to_coq(kernel, type) + ".\n\n");
                    }

                    return true;
                }
                else {
                    output << "Error: the assumption is not valid." << endl;
                    output << "In the command: " << ast.to_string() << endl;
                    return false;
                }
            }
            else if (ast.head == "Check") {
                // Check(x)
                if (ast.children.size() == 1) {
                    try {
                        auto term = kernel.parse(ast.children[0]);
                        auto type = kernel.calc_type(term);
                        output << kernel.term_to_string(term) << " : " << kernel.term_to_string(type) << endl;

                        // generate Coq code
                        if (gen_coq) {
                            append_coq_code("(* " + ast.to_string() + " *)\n");
                            append_coq_code("Check " + term_to_coq(kernel, term) + ".\n\n");
                        }
                        
                        return true;
                    }
                    catch (const exception& e) {
                        output << "Error: " << e.what() << endl;
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
                        if (find_def == nullopt) {
                            output << "Error: the symbol '" << name << "' is not defined." << endl;
                            return false;
                        }
                        output << kernel.dec_to_string(name, find_def.value()) << endl;

                        // generate Coq code
                        if (gen_coq) {
                            append_coq_code("(* " + ast.to_string() + " *)\n");
                            append_coq_code("Print " + name + ".\n\n");
                        }

                        return true;
                    }
                    catch (const exception& e) {
                        output << "Error: " << e.what() << endl;
                        return false;
                    }
                }
            }
            // ShowAll
            else if (ast.head == "ShowAll") {
                if (ast.children.size() == 0) {
                    output << "Environment:" << endl;
                    output << kernel.env_to_string() << endl;

                    // generate Coq code
                    if (gen_coq) {
                        append_coq_code("(* " + ast.to_string() + " *)\n");
                        append_coq_code("Print All.\n\n");
                    }
                    
                    return true;
                }
                else {
                    output << "Error: the command is not valid." << endl;
                    output << "In the command: " << ast.to_string() << endl;
                    return false;
                }
            }
            else if (ast.head == "Normalize") {
                if (ast.children.size() != 1) {
                    output << "Error: Normalize command should have one argument." << endl;
                    return false;
                }
                // Typecheck the term
                auto term = static_cast<const NormalTerm<int>*>(kernel.parse(ast.children[0]));
                kernel.calc_type(term);

                // calculate the normalized term
                vector<PosReplaceRecord> trace;
                auto normalized_term = pos_rewrite_repeated(kernel, term, rules, &trace);

                auto type = kernel.calc_type(normalized_term);
                
                // Output the normalized term
                output << kernel.term_to_string(normalized_term) + " : " + kernel.term_to_string(type)  << endl;


                // generate Coq code
                if (gen_coq) {
                    append_coq_code("(* " + ast.to_string() + " *)\n");
                    append_coq_code(normalize_to_coq(kernel, term, normalized_term, trace) + "\n\n");
                }

                return true;
            }
        }
        catch (const exception& e) {
            output << "Error: " << e.what() << endl;
            return false;
        }

        // bad command
        output << "Error: the command is not valid." << endl;
        output << "In the command: " << ast.to_string() << endl;
        return false;
    }

} // namespace diracoq
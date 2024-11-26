#include "diracoq.hpp"

namespace diracoq {

    using namespace std;
    using namespace ualg;

    std::optional<Declaration> Kernel::find_in_env(int symbol) {
        for (const auto& [sym, def] : env) {
            if (sym == symbol) {
                return def;
            }
        }
        return std::nullopt;
    }

    std::string Kernel::dec_to_string(const std::string& name, const Declaration& dec) const {
        if (dec.is_def()) {
            return name + " := " + term_to_string(*dec.def) + " : " + term_to_string(dec.type);
        }
        else {
            return name + " : " + term_to_string(dec.type);
        }
    }


    const Term<int>* Kernel::parse(const std::string& code) {
        return ualg::parse(sig, bank, code);
    }

    const Term<int>* Kernel::parse(const astparser::AST& ast) {
        return ualg::parse(sig, bank, ast);
    }

    string Kernel::term_to_string(const Term<int>* term) const {
        return sig.term_to_string(term);
    }

    string Kernel::env_to_string() const {
        string res = "";
        for (const auto& [sym, def] : env) {
            res += dec_to_string(sig.get_name(sym), def) + "\n";
        }
        return res;
    }

    bool Kernel::is_type(const Term<int>* term) {
        auto type = calc_type(term);

        if (type->get_head() == TYPE || type->get_head() == BASE) {
            return true;
        }

        return false;
    }


    const Term<int>* Kernel::calc_type(const Term<int>* term) {
        ListArgs<int> args;

        // (Arrow)
        if (match_normal_head(term, ARROW, args)) {
            if (args.size() != 2) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed.");
            }
            if (!is_type(args[0])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[0]) + " is not a well-typed type.");
            }
            if (!is_type(args[1])) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the body " + sig.term_to_string(args[1]) + " is not a well-typed type.");
            }

            return bank.get_normal_term(TYPE, {});
        }

        // (Lam)
        if (match_normal_head(term, FUN, args)) {
            if (args.size() != 3) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed.");
            }

            // try to add the definition
            assum(args[0]->get_head(), args[1]);
            
            // calculate the type of the body
            auto type_body = calc_type(args[2]);
            env.pop_back();

            return bank.get_normal_term(ARROW, {args[1], type_body});
        }

        // (App)
        if (match_normal_head(term, APPLY, args)) {
            if (args.size() != 2) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed.");
            }

            // calculate the type of f, which should be Arrow(T, U)
            auto type_f = calc_type(args[0]);

            ListArgs<int> args_f;
            if (match_normal_head(type_f, ARROW, args_f)) {
        
                // check whether the type of u matches
                if (!type_check(args[1], args_f[0])) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[1]) + " does not match the type of the function argument " + sig.term_to_string(args_f[0]) + ".");
                }

                return args_f[1];
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the function " + sig.term_to_string(args[0]) + " is not an arrow type.");
        }
        // (Type-Scalar)
        if (match_normal_head(term, SType, args)) {
            if (args.size() != 0) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because it has arguments.");
            }
            return bank.get_normal_term(TYPE, {});
        }
        // (Sca-0)
        if (match_normal_head(term, ZERO, args)) {
            return bank.get_normal_term(SType, {});
        }
        // (Sca-1)
        if (match_normal_head(term, ONE, args)) {
            return bank.get_normal_term(SType, {});
        }
        // (Sca-Delta) TODO
        // (Sca-Add)
        if (match_normal_head(term, ADDS, args)) {
            auto SType_term = bank.get_normal_term(SType, {});
            if (args.size() == 0) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because it has no arguments.");
            }
            for (const auto& arg : args) {
                if (!type_check(arg, SType_term)) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(arg) + " is not of type SType.");
                }
            }
            return SType_term;
        }
        // (Sca-Mul)
        if (match_normal_head(term, MLTS, args)) {
            auto SType_term = bank.get_normal_term(SType, {});
            if (args.size() == 0) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because it has no arguments.");
            }
            for (const auto& arg : args) {
                if (!type_check(arg, SType_term)) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(arg) + " is not of type SType.");
                }
            }
            return SType_term;
        }
        // (Sca-Conj)
        if (match_normal_head(term, CONJ, args)) {
            auto SType_term = bank.get_normal_term(SType, {});
            if (args.size() != 1) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument number is not 1.");
            }
            if (!type_check(args[0], SType_term)) {
                throw runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the argument " + sig.term_to_string(args[0]) + " is not of type SType.");
            }
            return SType_term;
        }
        // (Sca-Dot) TODO

        if (term->is_atomic()) {

            // (Const)
            auto env_find = find_in_env(term->get_head());
            if (env_find != std::nullopt) {
                return env_find->type;
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not assumed or defined.");
        }

        throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' cannot be typed.");
    }

    bool Kernel::type_check(const Term<int>* term, const Term<int>* type) {
        auto type_term = calc_type(term);

        if (type_term == type) {
            return true;
        }        

        if (type->get_head() == TYPE && type_term->get_head() == BASE) {
            return true;
        }

        return false;
    }

    void Kernel::assum(int symbol, const Term<int>* type) {
        auto TYPE_term = bank.get_normal_term(TYPE, {});

        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is reserved.");
        }
        if (find_in_env(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is already in the environment.");
        }

        // W-Assum-Type
        if (type == TYPE_term) {
            env.push_back({symbol, {std::nullopt, type}});
        }

        // W-Assum-Term
        else {
            if (!type_check(type, bank.get_normal_term(TYPE, {}))) {
                throw std::runtime_error("The type of the symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is not a well-typed type.");
            }
            env.push_back({symbol, {std::nullopt, type}});
        }
    }

    void Kernel::def(int symbol, const Term<int>* term, std::optional<const ualg::Term<int>*> type) {
        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is reserved.");
        }
        if (find_in_env(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is already in the environment."); 
        }
        if (type.has_value()) {
            if (!type_check(term, type.value())) {
                throw std::runtime_error("The term '" + sig.term_to_string(term) + "' is not well-typed with the type '" + sig.term_to_string(type.value()) + "'.");
            }
        }
        else {
            type = calc_type(term);
        }
        env.push_back({symbol, {term, type.value()}});
    }

    void Kernel::env_pop() {
        if (env.size() == 0) {
            throw std::runtime_error("The context is empty.");
        }
        env.pop_back();
    }

} // namespace diracoq
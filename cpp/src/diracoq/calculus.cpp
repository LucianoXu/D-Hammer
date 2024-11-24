#include "diracoq.hpp"

namespace diracoq {

    using namespace std;
    using namespace ualg;

    auto TYPE = CoC_sig.get_repr("Type");
    auto FORALL = CoC_sig.get_repr("forall");
    auto FUN = CoC_sig.get_repr("fun");
    auto APPLY = CoC_sig.get_repr("apply");
    auto BASE = CoC_sig.get_repr("Base");

    std::optional<Definition> Kernel::find_in_context(int symbol) {
        for (const auto& [sym, def] : context) {
            if (sym == symbol) {
                return def;
            }
        }
        return std::nullopt;
    }

    std::optional<Definition> Kernel::find_in_env(int symbol) {
        for (const auto& [sym, def] : env) {
            if (sym == symbol) {
                return def;
            }
        }
        return std::nullopt;
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
            if (def.is_def()) {
                res += sig.get_name(sym) + " := " + term_to_string(*def.def) + " : " + term_to_string(def.type) + "\n";
            }
            else {
                res += sig.get_name(sym) + " : " + term_to_string(def.type) + "\n";
            }
        }
        return res;
    }

    string Kernel::context_to_string() const {
        string res = "";
        for (const auto& [sym, def] : context) {
            if (def.is_def()) {
                res += sig.get_name(sym) + " := " + term_to_string(*def.def) + " : " + term_to_string(def.type) + "\n";
            }
            else {
                res += sig.get_name(sym) + " : " + term_to_string(def.type) + "\n";
            }
        }
        return res;
    }

    bool Kernel::is_sort(const Term<int>* term) {
        if (term->get_head() == TYPE || term->get_head() == BASE) {
            return true;
        }
        return false;
    }

    const Term<int>* Kernel::max_Type(const Term<int>* type1, const Term<int>* type2) {
        ListArgs<int> args1, args2;
        if (match_normal_head(type1, TYPE, args1) && match_normal_head(type2, TYPE, args2)) {
            if (args1.size() == 0) {
                return type2;
            }
            if (args2.size() == 0) {
                return type1;
            }

            return bank.get_normal_term(TYPE, {max_Type(args1[0], args2[0])});
        }
        
        throw std::runtime_error("Typing error: the term '" + sig.term_to_string(type1) + "' and the term '" + sig.term_to_string(type2) + "' are not both 'Type's.");
    }


    const Term<int>* Kernel::calc_type(const Term<int>* term) {
        ListArgs<int> args;

        // (Ax-Type)
        if (match_normal_head(term, TYPE, args)) {
            if (args.size() == 0) {
                return bank.get_normal_term(TYPE, {term});
            }
            if (args.size() > 1) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed.");
            }

            calc_type(args[0]);
            return bank.get_normal_term(TYPE, {term});
        }

        if (term->is_atomic()) {
            // check the reserved symbols
            if (is_reserved(term->get_head())) {
                throw std::runtime_error("Typing error: the symbol '" + sig.term_to_string(term) + "' is reserved.");
            }

            // (Var)
            auto context_find = find_in_context(term->get_head());
            if (context_find != std::nullopt) {
                return context_find->type;
            }

            // (Const)
            auto env_find = find_in_env(term->get_head());
            if (env_find != std::nullopt) {
                return env_find->type;
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not assumed or defined.");
        }

        // (Prod-Type) forall(x T U)
        if (match_normal_head(term, FORALL, args)) {
            if (args.size() != 3) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed.");
            }
            // calculate the type of T
            auto type_T = calc_type(args[1]);

            // calculate the type of the body
            context.push_back({args[0]->get_head(), {std::nullopt, args[1]}});
            auto type_U = calc_type(args[2]);
            context.pop_back();

            return max_Type(type_T, type_U);
        }

        // (Lam) fun(x T t)
        if (match_normal_head(term, FUN, args)) {
            if (args.size() != 3) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed.");
            }
            
            // calculate the type of t
            context.push_back({args[0]->get_head(), {std::nullopt, args[1]}});
            auto type_t = calc_type(args[2]);
            context.pop_back();

            // calculate the type of forall(x T U)
            auto term_forall = bank.get_normal_term(FORALL, {args[0], args[1], type_t});
            auto type_forall = calc_type(term_forall);
            if (!is_sort(type_forall)) {
                throw std::runtime_error("Typing error: the lambda term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the lambda term " + sig.term_to_string(term_forall) + " is not a well-typed type.");
            }

            return term_forall;
        }

        // (App) apply(f x)
        if (match_normal_head(term, APPLY, args)) {
            if (args.size() != 2) {
                throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed.");
            }

            // calculate the type of f, which should be forall(x U T)
            auto type_f = calc_type(args[0]);

            ListArgs<int> args_f;
            if (match_normal_head(type_f, FORALL, args_f)) {
                
                // calculate the type of u
                auto type_u = calc_type(args[1]);

                if (type_u != args_f[1]) {
                    throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the argument " + sig.term_to_string(args[1]) + " does not match the type of the function argument " + sig.term_to_string(args_f[1]) + ".");
                }

                // substitute x with u in T
                return bank.replace_term(args_f[2], {{args_f[0], args[1]}});
            }

            throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed, because the type of the function " + sig.term_to_string(args[0]) + " is not a function type.");
        }


        throw std::runtime_error("Unexpected error of type calculation on the term '" + sig.term_to_string(term) + "'.");
    }

    bool Kernel::coc_eq(const Term<int>* term1, const Term<int>* term2) {
        // TODO: implement the conversion and reduction rules in CoC.
        return term1 == term2;
    }

    bool Kernel::subtyping(const Term<int>* type1, const Term<int>* type2) {
        if (coc_eq(type1, type2)) return true;

        // case of two types are 'Type's
        ListArgs<int> args1, args2;
        if (match_normal_head(type1, TYPE, args1) && match_normal_head(type2, TYPE, args2)) {

            if (args1.size() == 0) return true;
            if (args2.size() == 0) return false;

            return subtyping(args1[0], args2[0]);
        }

        // the law for forall still needs to be implemented

        // // case of two types are 'forall(x T T')' and 'forall(x U U')'
        // if (match_normal_head(type1, FORALL, args1) && match_normal_head(type2, FORALL, args2)) {
        //     // Check T and U are coc-equivalent
        //     if (coc_eq(args1[1], args2[1])) {
        //         // Check T' is a subtype of U'
        //         context[args1[
        // }

        return false;
    }

    bool Kernel::type_check(const Term<int>* term, const Term<int>* type) {
        return subtyping(calc_type(term), type);
    }


    void Kernel::local_assum(int symbol, const Term<int>* type) {
        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is reserved.");
        }
        if (find_in_context(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is already in the context.");
        }
        if (!is_sort(calc_type(type))) {
            throw std::runtime_error("The type of the symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is not a well-typed type.");
        }
        context.push_back({symbol, {std::nullopt, type}});
    }

    void Kernel::local_def(int symbol, const Term<int>* term, std::optional<const ualg::Term<int>*> type) {
        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is reserved.");
        }
        if (find_in_context(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is already in the context."); 
        }
        if (type.has_value()) {
            if (!type_check(term, type.value())) {
                throw std::runtime_error("The term '" + sig.term_to_string(term) + "' is not well-typed with the type '" + sig.term_to_string(type.value()) + "'.");
            }
        }
        else {
            type = calc_type(term);
        }
        context.push_back({symbol, {term, type.value()}});
    }

    void Kernel::global_assum(int symbol, const Term<int>* type) {
        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is reserved.");
        }
        if (context.size() > 0) {
            throw std::runtime_error("The global assumption should be made in the empty context.");
        }
        if (find_in_env(symbol) != std::nullopt) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is already in the environment.");
        }
        if (!is_sort(calc_type(type))) {
            throw std::runtime_error("The type of the symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is not a well-typed type.");
        }
        env.push_back({symbol, {std::nullopt, type}});
    }

    void Kernel::gloabl_def(int symbol, const Term<int>* term, std::optional<const ualg::Term<int>*> type) {
        if (is_reserved(symbol)) {
            throw std::runtime_error("The symbol '" + sig.term_to_string(bank.get_normal_term(symbol, {})) + "' is reserved.");
        }
        if (context.size() > 0) {
            throw std::runtime_error("The global definition should be made in the empty context.");
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

    void Kernel::local_pop() {
        if (context.size() == 0) {
            throw std::runtime_error("The context is empty.");
        }
        context.pop_back();
    }

    void Kernel::local_clear() {
        context.clear();
    }

} // namespace diracoq
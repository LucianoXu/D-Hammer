
#include "symbols.hpp"
#include "ualg.hpp"

namespace diracoq {
    inline bool free_in(ualg::TermPtr<int> term, int var) {
        auto head = term->get_head();
        if (head == var) {
            return false;
        }
        auto& args = term->get_args();
        if (head == IDX || head == FORALL) {
            auto& args = term->get_args();
            if (args[0]->get_head() == var) {
                return true;
            }
        }
        if (head == FUN) {
            auto& args = term->get_args();
            if (args[0]->get_head() == var) {
                return free_in(args[1], var);
            }
        }

        for (const auto& arg : args) {
            if (!free_in(arg, var)) {
                return false;
            }
        }
        return true;
    }

    inline ualg::TermPtr<int> subst(ualg::Signature<int>& sig, ualg::TermPtr<int> term, int var, ualg::TermPtr<int> replacement) {
        auto head = term->get_head();

        if (head == var) {
            return replacement;
        }
        
        auto& args = term->get_args();
        if (head == IDX || head == FORALL) {
            auto& bound_var = term->get_args()[0]->get_head();
            if (bound_var == var) {
                return term;
            }
            // rename the bound variable if it is not free in the replacement
            if (!free_in(replacement, bound_var)) {
                auto new_bound_var = sig.register_symbol(sig.unique_var());
                auto renamed_body = subst(sig, term->get_args()[1], bound_var, create_term(new_bound_var));
                
                return create_term(head, {create_term(new_bound_var), subst(sig, renamed_body, var, replacement)});
            }
        }
        
        if (head == FUN) {
            auto& args = term->get_args();
            auto& bound_var = args[0]->get_head();
            if (bound_var == var) {
                return create_term(head, {args[0], subst(sig, args[1], var, replacement), args[2]});
            }

            if (!free_in(replacement, bound_var)) {
                auto new_bound_var = sig.register_symbol(sig.unique_var());
                auto renamed_body = subst(sig, args[2], bound_var, create_term(new_bound_var));

                return create_term(head, {args[0], subst(sig, args[1], var, replacement), subst(sig, renamed_body, var, replacement)});
            }
        }
    
        auto new_args = ualg::ListArgs<int>();
        for (const auto& arg : args) {
            new_args.push_back(subst(sig, arg, var, replacement));
        }
        return create_term(head, std::move(new_args));
    }

    /**
     * @brief Recursively transform a term to the de Bruijn index representation.
     * 
     * @param sig 
     * @param term 
     * @return ualg::TermPtr<int> 
     */
    ualg::TermPtr<int> to_deBruijn(ualg::Signature<int>& sig, ualg::TermPtr<int> term);

    inline bool is_eq_modulo_rset(ualg::TermPtr<int> termA, ualg::TermPtr<int> termB) {
        if (termA->get_head() != termB->get_head()) {
            return false;
        }
        auto argsA = termA->get_args();
        auto argsB = termB->get_args();

        if (argsA.size() != argsB.size()) {
            return false;
        }
        // consider the case of RSET
        if (termA->get_head() == RSET) {
            std::sort(argsA.begin(), argsA.end(), [](const auto& a, const auto& b) {
                return a->get_head() < b->get_head();
            });
            std::sort(argsB.begin(), argsB.end(), [](const auto& a, const auto& b) {
                return a->get_head() < b->get_head();
            });
        }
        for (int i = 0; i < argsA.size(); i++) {
            if (!is_eq_modulo_rset(argsA[i], argsB[i])) {
                return false;
            }
        }
        return true;
    }
        

    inline bool is_eq(ualg::Signature<int>& sig, ualg::TermPtr<int> termA, ualg::TermPtr<int> termB) {
        return is_eq_modulo_rset(to_deBruijn(sig, termA), to_deBruijn(sig, termB));
    }

} // namespace diracoq
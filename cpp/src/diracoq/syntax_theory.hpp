
#include "symbols.hpp"
#include "ualg.hpp"

namespace diracoq {
    inline bool free_in(const ualg::Term<int>* term, int var) {
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

    inline const ualg::Term<int>* subst(ualg::Signature<int>& sig, ualg::TermBank<int>& bank, const ualg::Term<int>* term, int var, const ualg::Term<int>* replacement) {
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
                auto new_bound_var = sig.register_symbol(unique_var());
                auto renamed_body = subst(sig, bank, term->get_args()[1], bound_var, bank.get_term(new_bound_var));
                
                return bank.get_term(head, {bank.get_term(new_bound_var), subst(sig, bank, renamed_body, var, replacement)});
            }
        }
        
        if (head == FUN) {
            auto& args = term->get_args();
            auto& bound_var = args[0]->get_head();
            if (bound_var == var) {
                return bank.get_term(head, {args[0], subst(sig, bank, args[1], var, replacement), args[2]});
            }

            if (!free_in(replacement, bound_var)) {
                auto new_bound_var = sig.register_symbol(unique_var());
                auto renamed_body = subst(sig, bank, args[2], bound_var, bank.get_term(new_bound_var));

                return bank.get_term(head, {args[0], subst(sig, bank, args[1], var, replacement), subst(sig, bank, renamed_body, var, replacement)});
            }
        }
    
        auto new_args = ualg::ListArgs<int>();
        for (const auto& arg : args) {
            new_args.push_back(subst(sig, bank, arg, var, replacement));
        }
        return bank.get_term(head, std::move(new_args));
    }

    inline bool alpha_eq(ualg::Signature<int>& sig, ualg::TermBank<int>& bank, const ualg::Term<int>* termA, const ualg::Term<int>* termB) {
        if (termA == termB) {
            return true;
        }

        auto headA = termA->get_head();

        if (headA != termB->get_head()) {
            return false;
        }

        // termA and termB have the same head
        auto& argsA = termA->get_args();
        auto& argsB = termB->get_args();
        if (headA == IDX || headA == FORALL) {
            auto new_var = sig.register_symbol(unique_var());
            return alpha_eq(sig, bank, 
                subst(sig, bank, argsA[1], argsA[0]->get_head(), bank.get_term(new_var)),
                subst(sig, bank, argsB[1], argsB[0]->get_head(), bank.get_term(new_var))
            );
        }

        if (headA == FUN) {
            if (!alpha_eq(sig, bank, argsA[1], argsB[1])) {
                return false;
            }
            
            auto new_var = sig.register_symbol(unique_var());
            return alpha_eq(sig, bank, 
                subst(sig, bank, argsA[2], argsA[0]->get_head(), bank.get_term(new_var)),
                subst(sig, bank, argsB[2], argsB[0]->get_head(), bank.get_term(new_var))
            );
        }

        // argsA and argsB have the same length
        for (int i = 0; i < argsA.size(); ++i) {
            if (!alpha_eq(sig, bank, argsA[i], argsB[i])) {
                return false;
            }
        }

        return true;
    }


    /**
     * @brief Recursively transform a term to the de Bruijn index representation.
     * 
     * @param sig 
     * @param bank 
     * @param term 
     * @return const ualg::Term<int>* 
     */
    const ualg::Term<int>* to_deBruijn(ualg::Signature<int>& sig, ualg::TermBank<int>& bank, const ualg::Term<int>* term);

    inline bool is_eq(ualg::Signature<int>& sig, ualg::TermBank<int>& bank, const ualg::Term<int>* termA, const ualg::Term<int>* termB) {
        return to_deBruijn(sig, bank, termA) == to_deBruijn(sig, bank, termB);
    }

    inline bool is_smaller(ualg::Signature<int>& sig, ualg::TermBank<int>& bank, const ualg::Term<int>* termA, const ualg::Term<int>* termB) {
        return to_deBruijn(sig, bank, termA) < to_deBruijn(sig, bank, termB);
    }

} // namespace diracoq
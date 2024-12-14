#include "diracoq.hpp"

// NOTE: the index of de Bruijn variables is 0-based, and is preserved as the head string of the term. The first 1024 integers are reserved for the de Bruijn indices. In other words, they have consistent name and representation.

namespace diracoq {
    using namespace ualg;

    const ualg::NormalTerm<int>* _bound_index_push(TermBank<int>& bank, const ualg::NormalTerm<int>* term) {
        if (is_deBruijn_index(term->get_head())) {
            unsigned varno = term->get_head();
            return bank.get_normal_term(varno + 1, {});
        }

        ListArgs<int> args;
        for (const auto& arg : term->get_args()) {
            args.push_back(_bound_index_push(bank, static_cast<const NormalTerm<int>*>(arg)));
        }

        return bank.get_normal_term(term->get_head(), std::move(args));
    }

    const NormalTerm<int>* deBruijn_replace(TermBank<int>& bank, const ualg::Term<int>* term, int index, const ualg::Term<int>* t) {
        
        auto term_head = term->get_head();

        auto norm_term = static_cast<const NormalTerm<int>*>(term);
        auto norm_t = static_cast<const NormalTerm<int>*>(t);

        if (is_deBruijn_index(term_head)) {
            if (term_head == index) {
                return norm_t;
            }
            else if (term_head > index) {
                return bank.get_normal_term(term_head - 1, {});
            }
            else {
                return norm_term;
            }
        }

        ListArgs<int> new_args;
        auto& args = norm_term->get_args();
        if (term_head == FUN || term_head == FORALL) {
            if (args.size() == 2) {
                new_args.push_back(deBruijn_replace(bank, args[0], index, t));
                new_args.push_back(deBruijn_replace(bank, args[1], index+1, _bound_index_push(bank, norm_t)));
            }
            else {
                new_args.push_back(deBruijn_replace(bank, args[0], index+1, _bound_index_push(bank, norm_t)));
            }
        }
        else {
            for (const auto& arg : norm_term->get_args()) {
                new_args.push_back(deBruijn_replace(bank, arg, index, t));
            }
        }

        return bank.get_normal_term(term_head, std::move(new_args));
    }

    const NormalTerm<int>* deBruijn_adjust(TermBank<int>& bank, const Term<int>* term, int index, int adjustment) {
        auto term_head = term->get_head();
        auto norm_term = static_cast<const NormalTerm<int>*>(term);
        if (is_deBruijn_index(term_head)) {
            if (term_head >= index) {
                return bank.get_normal_term(term_head + adjustment, {});
            }
            else {
                return norm_term;
            }
        }

        auto& args = norm_term->get_args();
        ListArgs<int> new_args;

        if (term_head == FUN || term_head == FORALL) {
            if (args.size() == 2) {
                new_args.push_back(deBruijn_adjust(bank, args[0], index, adjustment));
                new_args.push_back(deBruijn_adjust(bank, args[1], index+1, adjustment));
            }
            else {
                new_args.push_back(deBruijn_adjust(bank, args[0], index+1, adjustment));
            }
        }
        else {
            for (const auto& arg : norm_term->get_args()) {
                new_args.push_back(deBruijn_adjust(bank, arg, index, adjustment));
            }
        }

        return bank.get_normal_term(term_head, std::move(new_args));
    }


    const NormalTerm<int>* instantiate(TermBank<int>& bank, const Term<int>* term, int index, const Term<int>* t) {
        auto new_t = deBruijn_adjust(bank, t, index+1, -1);
        return deBruijn_replace(bank, term, index, new_t);
    }



    /**
     * @brief Check whether the given index is free in the term.
     * 
     * @param index 
     * @param term 
     * @return true 
     * @return false 
     */
    bool deBruijn_index_free_in(int index, const ualg::Term<int>* term) {
        if (is_deBruijn_index(term->get_head())) {
            return term->get_head() != index;
        }

        auto norm_term = static_cast<const NormalTerm<int>*>(term);
        auto& args = norm_term->get_args();
        if (term->get_head() == FUN || term->get_head() == FORALL) {
            if (args.size() == 2) {
                if (!deBruijn_index_free_in(index, args[0])) {
                    return false;
                }
                if (!deBruijn_index_free_in(index+1, args[1])) {
                    return false;
                }
            }
            else {
                if (!deBruijn_index_free_in(index+1, args[0])) {
                    return false;
                }
            }
        }
        else {
            for (const auto& arg : args) {
                if (!deBruijn_index_free_in(index, arg)) {
                    return false;
                }
            }
        }

        return true;
    }

    const NormalTerm<int>* deBruijn_swap(TermBank<int>& bank, const Term<int>* term, int i, int j) {
        auto term_head = term->get_head();

        if (is_deBruijn_index(term_head)) {
            if (term_head == i) {
                return bank.get_normal_term(j, {});
            }
            else if (term_head == j) {
                return bank.get_normal_term(i, {});
            }
            else {
                return static_cast<const NormalTerm<int>*>(term);
            }
        }

        ListArgs<int> new_args;
        auto norm_term = static_cast<const NormalTerm<int>*>(term);
        auto& args = norm_term->get_args();
        if (term_head == FUN || term_head == FORALL) {
            if (args.size() == 2) {
                new_args.push_back(deBruijn_swap(bank, args[0], i, j));
                new_args.push_back(deBruijn_swap(bank, args[1], i+1, j+1));
            }
            else {
                new_args.push_back(deBruijn_swap(bank, args[0], i+1, j+1));
            }
        }
        else {
            for (const auto& arg : args) {
                new_args.push_back(deBruijn_swap(bank, arg, i, j));
            }
        }

        return bank.get_normal_term(term_head, std::move(new_args));
    }

} // namespace diracoq
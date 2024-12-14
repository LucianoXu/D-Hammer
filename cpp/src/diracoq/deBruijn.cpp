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
        const NormalTerm<int>* new_t = norm_t;

        unsigned new_index = index;
        if (term_head == FUN || term_head == FORALL) {
            new_index++; 
            new_t = _bound_index_push(bank, norm_t);
        }


        for (const auto& arg : norm_term->get_args()) {
            new_args.push_back(deBruijn_replace(bank, arg, new_index, new_t));
        }

        return bank.get_normal_term(term_head, std::move(new_args));
    }

    const NormalTerm<int>* deBruijn_adjust(TermBank<int>& bank, const Term<int>* term, int index, int adjustment) {
        auto term_head = term->get_head();
        if (is_deBruijn_index(term_head)) {
            if (term_head >= index) {
                return bank.get_normal_term(term_head + adjustment, {});
            }
            else {
                return static_cast<const NormalTerm<int>*>(term);
            }
        }

        auto new_index = index;
        if (term_head == FUN || term_head == FORALL) {
            new_index++;
        }

        ListArgs<int> new_args;
        for (const auto& arg : static_cast<const NormalTerm<int>*>(term)->get_args()) {
            new_args.push_back(deBruijn_adjust(bank, static_cast<const NormalTerm<int>*>(arg), new_index, adjustment));
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

        int new_index = index;
        if (term->get_head() == FUN || term->get_head() == FORALL) {
            new_index++;
        }

        for (const auto& arg : static_cast<const NormalTerm<int>*>(term)->get_args()) {
            if (!deBruijn_index_free_in(new_index, arg)) {
                return false;
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
        auto new_i = i;
        auto new_j = j;
        if (term_head == FUN || term_head == FORALL) {
            new_i++;
            new_j++;
        }

        for (const auto& arg : static_cast<const NormalTerm<int>*>(term)->get_args()) {
            new_args.push_back(deBruijn_swap(bank, arg, new_i, new_j));
        }

        return bank.get_normal_term(term_head, std::move(new_args));
    }

} // namespace diracoq
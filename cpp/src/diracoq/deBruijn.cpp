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

    const NormalTerm<int>* _instantiate(TermBank<int>& bank, const ualg::NormalTerm<int>* term, const ualg::NormalTerm<int>* t, unsigned depth) {
        
        if (is_deBruijn_index(term->get_head())) {
            if (term->get_head() == depth) {
                return t;
            }
            else {
                return term;
            }
        }

        ListArgs<int> new_args;

        unsigned new_depth = depth;
        if (term->get_head() == FUN || term->get_head() == FORALL) {
            new_depth++;
        }

        const NormalTerm<int>* new_t = _bound_index_push(bank, t);

        for (const auto& arg : term->get_args()) {
            new_args.push_back(_instantiate(bank, static_cast<const NormalTerm<int>*>(arg), new_t, new_depth));
        }

        return bank.get_normal_term(term->get_head(), std::move(new_args));
    }

    const NormalTerm<int>* instantiate(TermBank<int>& bank, const NormalTerm<int>* term, const NormalTerm<int>* t) {
        return _instantiate(bank, term, t, 0);
    }

} // namespace diracoq
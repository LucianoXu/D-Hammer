#pragma once

#include <boost/unordered_set.hpp>

#include "term.hpp"

namespace ualg {

    template <class T>
    using TermMapping = std::map<const Term<T>*, const Term<T>*>;

    // The term bank. The signature of the term bank is fixed.
    template <class T>
    class TermBank {
    private:
        boost::unordered_set<Term<T>> terms;

    public:
        TermBank() {}

        unsigned int size() const;

        /**
         * @brief Get the atomic term object.
         * 
         * @param head 
         * @return const Term<T>* 
         */
        const Term<T>* get_term(const T& head);

        // Create a term from bank.
        // The properties of the symbols will be processed here.
        // NOTICE: make sure that all the subterms are already in the bank.
        const Term<T>* get_term(const T& head, ListArgs<T>&& args);

        // Create a term from bank.
        // The properties of the symbols will be processed here.
        // NOTICE: make sure that all the subterms are already in the bank.
        const Term<T>* get_term(const T& head, const ListArgs<T>& args);

        // Inductively construct a term in the bank. Check every subterms.
        // Inner terms are reconstructed first.
        const Term<T>* construct_term(const Term<T>& term);

    private:
        // Replace all the occurrences of p_old_term with p_new_term in the term.
        // mapping: the mapping from the old terms to the new terms
        const Term<T>* _replace_term(
            const Term<T>* term, 
            const TermMapping<T>& mapping,
            TermMapping<T>& cache);

    public:

        // Replace all the occurrences of p_old_term with p_new_term in the term.
        // mapping: the mapping from the old terms to the new terms
        // NOTICE: all terms should be in the bank.
        const Term<T>* replace_term(
            const Term<T>* term, 
            const TermMapping<T>& mapping);

        const Term<T>* replace_term_at(
            const Term<T>* term,
            const TermPos& pos,
            const Term<T>* new_subterm);
    };

    //////////////////////////////////////////////////////////////////////////
    // Implementation


    template <class T>
    unsigned int TermBank<T>::size() const {
        return terms.size();
    }

    template <class T>
    const Term<T>* TermBank<T>::get_term(const T& head) {
        auto term = Term<T>(head);
        auto p_find_res = terms.find(term);
        if (p_find_res != terms.end()) {
            return &(*p_find_res);
        }
        auto insert_result = terms.emplace(head);
        return &(*insert_result.first);
    }

    template <class T>
    const Term<T>* TermBank<T>::get_term(const T& head, ListArgs<T>&& args) {
        auto term = Term<T>(head, std::move(args));
        auto p_find_res = terms.find(term);
        if (p_find_res != terms.end()) {
            return &(*p_find_res);
        }
        auto insert_result = terms.emplace(head, std::move(term.get_args()));
        return &(*insert_result.first);
    }

    template <class T>
    const Term<T>* TermBank<T>::get_term(const T& head, const ListArgs<T>& args) {
        auto term = Term<T>(head, args);
        auto p_find_res = terms.find(term);
        if (p_find_res != terms.end()) {
            return &(*p_find_res);
        }
        auto insert_result = terms.emplace(head, args);
        return &(*insert_result.first);
    }

    template <class T>
    const Term<T>* TermBank<T>::construct_term(const Term<T>& term) {

        if (term.is_atomic()) {
            auto p_find_res = terms.find(term);
            if (p_find_res != terms.end()) {
                return &(*p_find_res);
            }
            auto insert_result = terms.emplace(term.get_head(), term.get_args());
            return &(*insert_result.first);
        }


        ListArgs<T> args;
        for (const auto& arg : term.get_args()) {
            args.push_back(construct_term(*arg));
        }

        return get_term(term.get_head(), std::move(args));
    }

    template <class T>
    const Term<T>* TermBank<T>::_replace_term(
        const Term<T>* term, 
        const TermMapping<T>& mapping,
        TermMapping<T>& cache) {

        // check if the term is within the mapping
        auto p_find_res = mapping.find(term);
        if (p_find_res != mapping.end()) {
            return p_find_res->second;
        }

        if (term->is_atomic()) {
            return term;
        }

        // Check the cached results
        p_find_res = cache.find(term);
        if (p_find_res != cache.end()) {
            return p_find_res->second;
        }

        // Inductively replace the subterms

        ListArgs<T> new_args;
        for (const auto& arg : term->get_args()) {
            new_args.push_back(_replace_term(arg, mapping, cache));
        }

        const Term<T>* new_term = get_term(term->get_head(), std::move(new_args));

        // Add to the cache
        cache[term] = new_term;
        return new_term;
    }

    template <class T>
    const Term<T>* TermBank<T>::replace_term(
        const Term<T>* term, 
        const TermMapping<T>& mapping){
        
        TermMapping<T> cache;
        return _replace_term(term, mapping, cache);
    }


    template <class T>
    const Term<T>* TermBank<T>::replace_term_at(
        const Term<T>* term,
        const TermPos& pos,
        const Term<T>* new_subterm) {

        if (pos.size() == 0) {
            return new_subterm;
        }

        ListArgs<T> new_args;
        for (unsigned int i = 0; i < term->get_args().size(); i++) {
            if (i == pos[0]) {
                auto new_term = replace_term_at(
                    term->get_args()[i],
                    TermPos(pos.begin() + 1, pos.end()),
                    new_subterm);

                new_args.push_back(new_term);
            }
            else {
                new_args.push_back(term->get_args()[i]);
            }
        }
        return get_term(term->get_head(), std::move(new_args));
    }


} // namespace ualg
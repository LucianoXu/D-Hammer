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
        boost::unordered_set<NormalTerm<T>> normal_terms;
        boost::unordered_set<CTerm<T>> c_terms;
        boost::unordered_set<ACTerm<T>> ac_terms;

    public:
        TermBank() {}

        unsigned int size() const;

        // Create a term from bank.
        // The properties of the symbols will be processed here.
        // NOTICE: make sure that all the subterms are already in the bank.
        const NormalTerm<T>* get_normal_term(const T& head, ListArgs<T>&& args);

        // Create a term from bank.
        // The properties of the symbols will be processed here.
        // NOTICE: make sure that all the subterms are already in the bank.
        const NormalTerm<T>* get_normal_term(const T& head, const ListArgs<T>& args);


        // Create a term from bank.
        const CTerm<T>* get_c_term(const T& head, const TermCountMapping<T>& args);

        const CTerm<T>* get_c_term(const T& head, TermCountMapping<T>&& args);
        

        // Create a term from bank.
        const ACTerm<T>* get_ac_term(const T& head, const TermCountMapping<T>& args);

        const ACTerm<T>* get_ac_term(const T& head, TermCountMapping<T>&& args);

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

        const NormalTerm<T>* replace_term(
            const NormalTerm<T>* term,
            const NormalTermPos& pos,
            const NormalTerm<T>* new_subterm);
    };

    //////////////////////////////////////////////////////////////////////////
    // Implementation


    template <class T>
    unsigned int TermBank<T>::size() const {
        return normal_terms.size() + ac_terms.size() + c_terms.size();
    }

    template <class T>
    const NormalTerm<T>* TermBank<T>::get_normal_term(const T& head, ListArgs<T>&& args) {
        auto term = NormalTerm<T>(head, std::move(args));
        auto p_find_res = normal_terms.find(term);
        if (p_find_res != normal_terms.end()) {
            return static_cast<const NormalTerm<T>*>(&(*p_find_res));
        }
        auto insert_result = normal_terms.insert(term);
        return static_cast<const NormalTerm<T>*>(&(*insert_result.first));
    }

    template <class T>
    const NormalTerm<T>* TermBank<T>::get_normal_term(const T& head, const ListArgs<T>& args) {
        auto term = NormalTerm<T>(head, args);
        auto p_find_res = normal_terms.find(term);
        if (p_find_res != normal_terms.end()) {
            return static_cast<const NormalTerm<T>*>(&(*p_find_res));
        }
        auto insert_result = normal_terms.insert(term);
        return static_cast<const NormalTerm<T>*>(&(*insert_result.first));
    }

    template <class T>
    const CTerm<T>* TermBank<T>::get_c_term(const T& head, const TermCountMapping<T>& args) {
        auto term = CTerm<T>(head, args);
        auto p_find_res = c_terms.find(term);
        if (p_find_res != c_terms.end()) {
            return static_cast<const CTerm<T>*>(&(*p_find_res));
        }
        auto insert_result = c_terms.insert(term);
        return static_cast<const CTerm<T>*>(&(*insert_result.first));
    }

    template <class T>
    const CTerm<T>* TermBank<T>::get_c_term(const T& head, TermCountMapping<T>&& args) {
        auto term = CTerm<T>(head, std::move(args));
        auto p_find_res = c_terms.find(term);
        if (p_find_res != c_terms.end()) {
            return static_cast<const CTerm<T>*>(&(*p_find_res));
        }
        auto insert_result = c_terms.insert(term);
        return static_cast<const CTerm<T>*>(&(*insert_result.first));
    }

    template <class T>
    const ACTerm<T>* TermBank<T>::get_ac_term(const T& head, const TermCountMapping<T>& args) {
        auto term = ACTerm<T>(head, args);
        auto p_find_res = ac_terms.find(term);
        if (p_find_res != ac_terms.end()) {
            return static_cast<const ACTerm<T>*>(&(*p_find_res));
        }
        auto insert_result = ac_terms.insert(term);
        return static_cast<const ACTerm<T>*>(&(*insert_result.first));
    }

    template <class T>
    const ACTerm<T>* TermBank<T>::get_ac_term(const T& head, TermCountMapping<T>&& args) {
        auto term = ACTerm<T>(head, std::move(args));
        auto p_find_res = ac_terms.find(term);
        if (p_find_res != ac_terms.end()) {
            return static_cast<const ACTerm<T>*>(&(*p_find_res));
        }
        auto insert_result = ac_terms.insert(term);
        return static_cast<const ACTerm<T>*>(&(*insert_result.first));
    }

    template <class T>
    const Term<T>* TermBank<T>::construct_term(const Term<T>& term) {
        if (typeid(term) == typeid(NormalTerm<T>)) {
            const NormalTerm<T>& normal_term = static_cast<const NormalTerm<T>&>(term);

            if (term.is_atomic()) {
                auto p_find_res = normal_terms.find(normal_term);
                if (p_find_res != normal_terms.end()) {
                    return &(*p_find_res);
                }
                auto insert_result = normal_terms.insert(normal_term);
                return &(*insert_result.first);
            }


            ListArgs<T> args;
            for (const auto& arg : normal_term.get_args()) {
                args.push_back(const_cast<const Term<T>*>(construct_term(*arg)));
            }

            return get_normal_term(term.get_head(), std::move(args));
        }

        else if (typeid(term) == typeid(CTerm<T>)) {
            const CTerm<T>& c_term = static_cast<const CTerm<T>&>(term);

            if (term.is_atomic()) {
                auto p_find_res = c_terms.find(c_term);
                if (p_find_res != c_terms.end()) {
                    return &(*p_find_res);
                }
                auto insert_result = c_terms.insert(c_term);
                return &(*insert_result.first);
            }

            TermCountMapping<T> args;
            for (const auto& arg : c_term.get_args()) {
                auto sub_construct_res = construct_term(*arg.first);
                add_TermCountMapping(args, sub_construct_res, arg.second);
            }

            return get_c_term(term.get_head(), std::move(args));
        }

        else if (typeid(term) == typeid(ACTerm<T>)) {
            const ACTerm<T>& ac_term = static_cast<const ACTerm<T>&>(term);

            if (term.is_atomic()) {
                auto p_find_res = ac_terms.find(ac_term);
                if (p_find_res != ac_terms.end()) {
                    return &(*p_find_res);
                }
                auto insert_result = ac_terms.insert(ac_term);
                return &(*insert_result.first);
            }

            TermCountMapping<T> args;
            for (const auto& arg : ac_term.get_args()) {
                auto sub_construct_res = construct_term(*arg.first);
                add_TermCountMapping(args, sub_construct_res, arg.second);
            }

            return get_ac_term(term.get_head(), std::move(args));

        }

        else {
            throw std::runtime_error("Unknown term type.");
        }
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
        if (typeid(*term) == typeid(NormalTerm<T>)) {
            const NormalTerm<T>* normal_term = static_cast<const NormalTerm<T>*>(term);

            ListArgs<T> new_args;
            for (const auto& arg : normal_term->get_args()) {
                new_args.push_back(_replace_term(arg, mapping, cache));
            }

            const Term<T>* new_term = get_normal_term(normal_term->get_head(), std::move(new_args));

            // Add to the cache
            cache[normal_term] = new_term;
            return new_term;
        }   
        else if (typeid(*term) == typeid(CTerm<T>)) {
            const CTerm<T>* c_term = static_cast<const CTerm<T>*>(term);

            TermCountMapping<T> new_args;
            for (const auto& arg : c_term->get_args()) {
                auto sub_construct_res = _replace_term(arg.first, mapping, cache);
                add_TermCountMapping(new_args, sub_construct_res, arg.second);
            }

            const Term<T>* new_term = get_c_term(c_term->get_head(), std::move(new_args));

            // Add to the cache
            cache[c_term] = new_term;
            return new_term;
        }
        else if (typeid(*term) == typeid(ACTerm<T>)) {
            const ACTerm<T>* ac_term = static_cast<const ACTerm<T>*>(term);

            TermCountMapping<T> new_args;
            for (const auto& arg : ac_term->get_args()) {
                auto sub_construct_res = _replace_term(arg.first, mapping, cache);
                add_TermCountMapping(new_args, sub_construct_res, arg.second);
            }

            const Term<T>* new_term = get_ac_term(ac_term->get_head(), std::move(new_args));

            // Add to the cache
            cache[ac_term] = new_term;
            return new_term;
        }
        else {
            throw std::runtime_error("Unknown term type.");
        }
    }

    template <class T>
    const Term<T>* TermBank<T>::replace_term(
        const Term<T>* term, 
        const TermMapping<T>& mapping){
        
        TermMapping<T> cache;
        return _replace_term(term, mapping, cache);
    }


    template <class T>
    const NormalTerm<T>* TermBank<T>::replace_term(
        const NormalTerm<T>* term,
        const NormalTermPos& pos,
        const NormalTerm<T>* new_subterm) {

        if (pos.size() == 0) {
            return new_subterm;
        }

        ListArgs<T> new_args;
        for (unsigned int i = 0; i < term->get_args().size(); i++) {
            if (i == pos[0]) {
                auto new_term = replace_term(
                    static_cast<const NormalTerm<T>*>(term->get_args()[i]),
                    NormalTermPos(pos.begin() + 1, pos.end()),
                    new_subterm);

                new_args.push_back(new_term);
            }
            else {
                new_args.push_back(term->get_args()[i]);
            }
        }
        return get_normal_term(term->get_head(), std::move(new_args));
    }


} // namespace ualg
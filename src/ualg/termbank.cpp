#include "ualg.hpp"

namespace ualg {

    unsigned int TermBank::size() const {
        return normal_terms.size();
    }

    const NormalTerm* TermBank::get_normal_term(const std::string& head, std::vector<const Term*>&& args) {
        auto term = NormalTerm(head, std::move(args));
        auto p_find_res = normal_terms.find(term);
        if (p_find_res != normal_terms.end()) {
            return static_cast<const NormalTerm*>(&(*p_find_res));
        }
        auto insert_result = normal_terms.insert(term);
        return static_cast<const NormalTerm*>(&(*insert_result.first));
    }

    const NormalTerm* TermBank::get_normal_term(const std::string& head, const std::vector<const Term*>& args) {
        auto term = NormalTerm(head, args);
        auto p_find_res = normal_terms.find(term);
        if (p_find_res != normal_terms.end()) {
            return static_cast<const NormalTerm*>(&(*p_find_res));
        }
        auto insert_result = normal_terms.insert(term);
        return static_cast<const NormalTerm*>(&(*insert_result.first));
    }

    const Term* TermBank::construct_term(const Term& term) {
        if (typeid(term) == typeid(NormalTerm)) {
            const NormalTerm& normal_term = static_cast<const NormalTerm&>(term);

            if (term.is_atomic()) {
                auto p_find_res = normal_terms.find(normal_term);
                if (p_find_res != normal_terms.end()) {
                    return &(*p_find_res);
                }
                auto insert_result = normal_terms.insert(normal_term);
                return &(*insert_result.first);
            }


            std::vector<const Term*> args;
            for (const auto& arg : normal_term.get_args()) {
                args.push_back(const_cast<const Term*>(construct_term(*arg)));
            }

            return get_normal_term(term.get_head(), std::move(args));
        }

        else {
            throw std::runtime_error("Unknown term type.");
        }
    }

    const Term* TermBank::_replace_term(
        const Term* term, 
        const TermMapping& mapping,
        TermMapping& cache) {

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
        if (typeid(*term) == typeid(NormalTerm)) {
            const NormalTerm* normal_term = static_cast<const NormalTerm*>(term);

            std::vector<const Term*> new_args;
            for (const auto& arg : normal_term->get_args()) {
                new_args.push_back(_replace_term(arg, mapping, cache));
            }

            const Term* new_term = get_normal_term(normal_term->get_head(), std::move(new_args));

            // Add to the cache
            cache[normal_term] = new_term;
            return new_term;
        }   
        else {
            throw std::runtime_error("Unknown term type.");
        }
    }

    const Term* TermBank::replace_term(
        const Term* term, 
        const TermMapping& mapping){
        
        TermMapping cache;
        return _replace_term(term, mapping, cache);
    }

} // namespace ualg
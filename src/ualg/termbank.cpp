#include "ualg.hpp"

namespace ualg {

    unsigned int TermBank::size() const {
        return normal_terms.size() + ac_terms.size() + c_terms.size();
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

    const CTerm* TermBank::get_c_term(const std::string& head, const TermCountMappping& args) {
        auto term = CTerm(head, args);
        auto p_find_res = c_terms.find(term);
        if (p_find_res != c_terms.end()) {
            return static_cast<const CTerm*>(&(*p_find_res));
        }
        auto insert_result = c_terms.insert(term);
        return static_cast<const CTerm*>(&(*insert_result.first));
    }

    const CTerm* TermBank::get_c_term(const std::string& head, TermCountMappping&& args) {
        auto term = CTerm(head, std::move(args));
        auto p_find_res = c_terms.find(term);
        if (p_find_res != c_terms.end()) {
            return static_cast<const CTerm*>(&(*p_find_res));
        }
        auto insert_result = c_terms.insert(term);
        return static_cast<const CTerm*>(&(*insert_result.first));
    }

    const ACTerm* TermBank::get_ac_term(const std::string& head, const TermCountMappping& args) {
        auto term = ACTerm(head, args);
        auto p_find_res = ac_terms.find(term);
        if (p_find_res != ac_terms.end()) {
            return static_cast<const ACTerm*>(&(*p_find_res));
        }
        auto insert_result = ac_terms.insert(term);
        return static_cast<const ACTerm*>(&(*insert_result.first));
    }

    const ACTerm* TermBank::get_ac_term(const std::string& head, TermCountMappping&& args) {
        auto term = ACTerm(head, std::move(args));
        auto p_find_res = ac_terms.find(term);
        if (p_find_res != ac_terms.end()) {
            return static_cast<const ACTerm*>(&(*p_find_res));
        }
        auto insert_result = ac_terms.insert(term);
        return static_cast<const ACTerm*>(&(*insert_result.first));
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

        else if (typeid(term) == typeid(CTerm)) {
            const CTerm& c_term = static_cast<const CTerm&>(term);

            if (term.is_atomic()) {
                auto p_find_res = c_terms.find(c_term);
                if (p_find_res != c_terms.end()) {
                    return &(*p_find_res);
                }
                auto insert_result = c_terms.insert(c_term);
                return &(*insert_result.first);
            }

            TermCountMappping args;
            for (const auto& arg : c_term.get_args()) {
                auto sub_construct_res = construct_term(*arg.first);
                update_TermCountMapping(args, sub_construct_res, arg.second);
            }

            return get_c_term(term.get_head(), std::move(args));
        }

        else if (typeid(term) == typeid(ACTerm)) {
            const ACTerm& ac_term = static_cast<const ACTerm&>(term);

            if (term.is_atomic()) {
                auto p_find_res = ac_terms.find(ac_term);
                if (p_find_res != ac_terms.end()) {
                    return &(*p_find_res);
                }
                auto insert_result = ac_terms.insert(ac_term);
                return &(*insert_result.first);
            }

            TermCountMappping args;
            for (const auto& arg : ac_term.get_args()) {
                auto sub_construct_res = construct_term(*arg.first);
                update_TermCountMapping(args, sub_construct_res, arg.second);
            }

            return get_ac_term(term.get_head(), std::move(args));

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
        else if (typeid(*term) == typeid(CTerm)) {
            const CTerm* c_term = static_cast<const CTerm*>(term);

            TermCountMappping new_args;
            for (const auto& arg : c_term->get_args()) {
                auto sub_construct_res = _replace_term(arg.first, mapping, cache);
                update_TermCountMapping(new_args, sub_construct_res, arg.second);
            }

            const Term* new_term = get_c_term(c_term->get_head(), std::move(new_args));

            // Add to the cache
            cache[c_term] = new_term;
            return new_term;
        }
        else if (typeid(*term) == typeid(ACTerm)) {
            const ACTerm* ac_term = static_cast<const ACTerm*>(term);

            TermCountMappping new_args;
            for (const auto& arg : ac_term->get_args()) {
                auto sub_construct_res = _replace_term(arg.first, mapping, cache);
                update_TermCountMapping(new_args, sub_construct_res, arg.second);
            }

            const Term* new_term = get_ac_term(ac_term->get_head(), std::move(new_args));

            // Add to the cache
            cache[ac_term] = new_term;
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
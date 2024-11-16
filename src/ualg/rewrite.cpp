#include "ualg.hpp"

namespace ualg {

    std::optional<TermMapping> get_mapping(TermBank& bank, const Term* term, const std::vector<RewritingRule>& rules) {

        // Check whether the rule can be applied to this term
        for (const auto& rule : rules) {
            auto apply_res = rule(bank, term);
            if (apply_res.has_value()) {
                // return the discovered mapping
                TermMapping mapping = {{term, apply_res.value()}};

                return std::move(mapping);
            }
        }
        
        // Check whether the rule can be applied to the subterms
        if (typeid(*term) == typeid(NormalTerm)) {
            const NormalTerm* normal_term = static_cast<const NormalTerm*>(term);

            for (const auto& arg : normal_term->get_args()) {
                auto mapping = get_mapping(bank, arg, rules);
                if (mapping.has_value()) {
                    return mapping;
                }
            }
        }

        else if (typeid(*term) == typeid(CTerm)) {
            const CTerm* c_term = static_cast<const CTerm*>(term);

            for (const auto& [arg, _] : c_term->get_args()) {
                auto mapping = get_mapping(bank, arg, rules);
                if (mapping.has_value()) {
                    return mapping;
                }
            }
        }

        else if (typeid(*term) == typeid(ACTerm)) {
            const ACTerm* ac_term = static_cast<const ACTerm*>(term);

            for (const auto& [arg, _] : ac_term->get_args()) {
                auto mapping = get_mapping(bank, arg, rules);
                if (mapping.has_value()) {
                    return mapping;
                }
            }
        }

        else {
            throw std::runtime_error("Unknown term type.");
        }

        return std::nullopt;
    }


    std::optional<const Term*> rewrite_all(TermBank& bank, const Term* term, const std::vector<RewritingRule>& rules) {

        auto mapping = get_mapping(bank, term, rules);

        if (!mapping.has_value()) {
            return std::nullopt;
        }

        return bank.replace_term(term, mapping.value());
    }

    const Term* rewrite_repeated(TermBank& bank, const Term* term, const std::vector<RewritingRule>& rules) {
        auto current_term = term;
        while (true) {
            auto rewritten_term = rewrite_all(bank, current_term, rules);
            if (!rewritten_term.has_value()) {
                return current_term;
            }
            current_term = rewritten_term.value();
        }
    }


} // namespace ualg
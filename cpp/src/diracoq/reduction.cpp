#include "diracoq.hpp"

namespace diracoq {

    using namespace ualg;
    using namespace std;


    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, const NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules, NormalTermPos& current_pos) {
        
        // Check whether the rule can be applied to this term
        for (const auto& rule : rules) {
            auto apply_res = rule(kernel, term);
            if (apply_res.has_value()) {
                // return the discovered replacement
                return PosReplaceRecord{rule, current_pos, static_cast<const NormalTerm<int>*>(apply_res.value())};
            }
        }
        
        // Check whether the rule can be applied to the subterms

        for (unsigned int i = 0; i < term->get_args().size(); i++) {
            current_pos.push_back(i);
            auto subterm = static_cast<const NormalTerm<int>*>(term->get_args()[i]);
            auto replace_res = get_pos_replace(kernel, subterm, rules, current_pos);
            if (replace_res.has_value()) {
                return replace_res;
            }
            current_pos.pop_back();
        }

        return std::nullopt;        
    }

    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, const NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules) {
        NormalTermPos current_pos;
        return get_pos_replace(kernel, term, rules, current_pos);
    }

    const NormalTerm<int>* pos_rewrite_repeated(Kernel& kernel, const NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules, std::vector<PosReplaceRecord>* trace) {
        auto current_term = term;
        while (true) {
            auto replace_res = get_pos_replace(kernel, current_term, rules);
            if (replace_res.has_value()) {
                if (trace != nullptr) {
                    trace->push_back(replace_res.value());
                }
                current_term = kernel.get_bank().replace_term(current_term, replace_res.value().pos, replace_res.value().replacement);
            }
            else {
                break;
            }
        }
        return current_term;
    }

    //////////////// Rules

    DIRACOQ_RULE_DEF(BETA, kernel, term) {
        ListArgs<int> args;
        if (match_normal_head(term, APPLY, args)) {
            ListArgs<int> fun_args;
            if (match_normal_head(args[0], FUN, fun_args)) {
                return kernel.get_bank().replace_term(fun_args[2], {{fun_args[0], args[1]}});
            }
        }
        return std::nullopt;
    }

    DIRACOQ_RULE_DEF(DELTA, kernel, term) {
        auto find_res = kernel.find_in_env(term->get_head());
        if (find_res != std::nullopt and find_res->is_def()) {
            return find_res->def.value();
        }
        return std::nullopt;
    }

    DIRACOQ_RULE_DEF(ETA, kernel, term) {
        ListArgs<int> args;
        if (match_normal_head(term, FUN, args)) {
            ListArgs<int> fun_args;
            if (match_normal_head(args[2], APPLY, fun_args)) {
                if (fun_args[1] == args[0]) {
                    return fun_args[0];
                }
            }
        }
        return std::nullopt;
    }


    const std::vector<PosRewritingRule> rules = {
        BETA,
        DELTA,
        ETA
    };    

} // namespace diracoq
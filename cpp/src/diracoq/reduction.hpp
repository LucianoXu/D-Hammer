#pragma once

#include "ualg.hpp"

namespace diracoq {

    // The rewriting rules of the Diracoq kernel.
    using PosRewritingRule = std::optional<const ualg::Term<int>*> (*)(Kernel& kernel, const ualg::Term<int>* term);

#define DIRACOQ_RULE_DEF(name, kernel, term) std::optional<const ualg::Term<int>*> name(diracoq::Kernel& kernel, const ualg::Term<int>* term)

    // The struct for the rewriting trace. It records which rule is applied, at which position, and the new replacement.
    struct PosReplaceRecord {
        PosRewritingRule rule;
        ualg::NormalTermPos pos;
        const ualg::NormalTerm<int>* replacement;
    };


    /**
     * @brief Get the rewriting record using the rewriting rules.
     * 
     * @param kernel 
     * @param term 
     * @param rules 
     * @return std::optional<PosReplaceRecord> 
     */
    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, const ualg::NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules);


    /**
     * @brief Rewrite the term repeatedly using the given rewriting rules, until no more rules can apply.
     * 
     * @param kernel 
     * @param term 
     * @param rules 
     * @param trace The container to store the trace of the rewriting.
     * @return const NormalTerm<int>* 
     */
    const ualg::NormalTerm<int>* pos_rewrite_repeated(Kernel& kernel, const ualg::NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules, 
    std::vector<PosReplaceRecord>* trace = nullptr);


    //////////////// Rules

    // beta reduction
    DIRACOQ_RULE_DEF(BETA, kernel, term);
    // delta reduction
    DIRACOQ_RULE_DEF(DELTA, kernel, term);
    // eta reduction
    DIRACOQ_RULE_DEF(ETA, kernel, term);

    // The scalar rule list.
    extern const std::vector<PosRewritingRule> rules;


    ///////////////// Trace Output

} // namespace scalar_vec
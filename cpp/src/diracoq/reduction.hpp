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


    //////////////// Flattening AC symbols
    DIRACOQ_RULE_DEF(R_FLATTEN, kernel, term);


    //////////////// properties of the symbols

    // ADDS(a) -> a
    DIRACOQ_RULE_DEF(R_ADDSID, kernel, term);
    // MLTS(a) -> a
    DIRACOQ_RULE_DEF(R_MLTSID, kernel, term);


    //////////////// rewriting rules

    // ADDS(a 0) -> a
    // This rule removes all 0s from the subterm
    DIRACOQ_RULE_DEF(R_ADDS0, kernel, term);

    // MLTS(a 0) -> 0
    DIRACOQ_RULE_DEF(R_MLTS0, kernel, term);

    // MLTS(a 1) -> a
    // This rule removes all 0s from the subterm
    DIRACOQ_RULE_DEF(R_MLTS1, kernel, term);

    // MLTS((seq1: __) ADDS(a1 a2 ... an) (seq2: __)) -> ADDS(MLTS(seq1 a1 seq2) MLTS(seq1 a2 seq2) ... MLTS(seq1 an seq2))
    // This rule expands on the first ocurrence of ADDS in the subterm
    DIRACOQ_RULE_DEF(R_MLTS2, kernel, term);

    // CONJ(0) -> 0
    DIRACOQ_RULE_DEF(R_CONJ0, kernel, term);

    // CONJ(1) -> 1
    DIRACOQ_RULE_DEF(R_CONJ1, kernel, term);

    // CONJ(ADDS(a b)) -> ADDS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ2, kernel, term);

    // CONJ(MLTS(a b)) -> MLTS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ3, kernel, term);

    // CONJ(CONJ(a)) -> a
    DIRACOQ_RULE_DEF(R_CONJ4, kernel, term);

    // The scalar rule list.
    extern const std::vector<PosRewritingRule> rules;


    ///////////////// Trace Output

} // namespace scalar_vec
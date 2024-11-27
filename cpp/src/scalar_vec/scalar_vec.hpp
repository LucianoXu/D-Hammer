#pragma once

#include "ualg.hpp"

namespace scalar_vec {

    extern ualg::StringSymbolType symbols;

    extern const ualg::Signature<int> reserved_sig;

    extern std::set<int> ac_symbols;


    //////////////// Flattening AC symbols
    REWRITE_COMPILED_DEF(R_FLATTEN, bank, term);


    //////////////// properties of the symbols

    // ADDS(a) -> a
    REWRITE_COMPILED_DEF(R_ADDSID, bank, term);
    // MULS(a) -> a
    REWRITE_COMPILED_DEF(R_MULSID, bank, term);


    //////////////// rewriting rules

    // ADDS(a 0) -> a
    // This rule removes all 0s from the subterm
    REWRITE_COMPILED_DEF(R_ADDS0, bank, term);

    // MULS(a 0) -> 0
    REWRITE_COMPILED_DEF(R_MULS0, bank, term);

    // MULS(a 1) -> a
    // This rule removes all 0s from the subterm
    REWRITE_COMPILED_DEF(R_MULS1, bank, term);

    // MULS((seq1: __) ADDS(a1 a2 ... an) (seq2: __)) -> ADDS(MULS(seq1 a1 seq2) MULS(seq1 a2 seq2) ... MULS(seq1 an seq2))
    // This rule expands on the first ocurrence of ADDS in the subterm
    REWRITE_COMPILED_DEF(R_MULS2, bank, term);

    // CONJ(0) -> 0
    REWRITE_COMPILED_DEF(R_CONJ0, bank, term);

    // CONJ(1) -> 1
    REWRITE_COMPILED_DEF(R_CONJ1, bank, term);

    // CONJ(ADDS(a b)) -> ADDS(CONJ(a) CONJ(b))
    REWRITE_COMPILED_DEF(R_CONJ2, bank, term);

    // CONJ(MULS(a b)) -> MULS(CONJ(a) CONJ(b))
    REWRITE_COMPILED_DEF(R_CONJ3, bank, term);

    // CONJ(CONJ(a)) -> a
    REWRITE_COMPILED_DEF(R_CONJ4, bank, term);

    // The scalar rule list.
    extern const std::vector<ualg::RewritingRule<int>> scalar_rules;

    // The dummy rule for the equality.
    REWRITE_COMPILED_DEF(R_C_EQ, bank, term);

    const ualg::Term<int>* normalize(ualg::TermBank<int>& bank, const ualg::Term<int>* term, ualg::RewritingTrace<int>* trace=nullptr);

    // facilities for output
    extern const std::map<ualg::RewritingRule<int>, std::string> scalar_rule_names;
    std::string scalar_printer(const ualg::Signature<int>& sig, const ualg::RewritingRecord<int>& record);

} // namespace scalar_vec
#pragma once

#include "ualg.hpp"

namespace scalar {

    extern ualg::StringSymbolType symbols;

    extern const ualg::IntSignature reserved_sig;

    //////////////// properties of the symbols

    // ADDS(a) -> a
    REWRITE_COMPILED_DEF(R_ADDSID, bank, term);
    // MLTS(a) -> a
    REWRITE_COMPILED_DEF(R_MLTSID, bank, term);


    //////////////// rewriting rules

    // ADDS(a 0) -> a
    REWRITE_COMPILED_DEF(R_ADDS0, bank, term);

    // MLTS(a 0) -> 0
    REWRITE_COMPILED_DEF(R_MLTS0, bank, term);

    // MLTS(a 1) -> a
    REWRITE_COMPILED_DEF(R_MLTS1, bank, term);

    // MLTS(a ADDS(b c)) -> ADDS(MLTS(a b) MLTS(a c))
    REWRITE_COMPILED_DEF(R_MLTS2, bank, term);

    // CONJ(0) -> 0
    REWRITE_COMPILED_DEF(R_CONJ0, bank, term);

    // CONJ(1) -> 1
    REWRITE_COMPILED_DEF(R_CONJ1, bank, term);

    // CONJ(ADDS(a b)) -> ADDS(CONJ(a) CONJ(b))
    REWRITE_COMPILED_DEF(R_CONJ2, bank, term);

    // CONJ(MLTS(a b)) -> MLTS(CONJ(a) CONJ(b))
    REWRITE_COMPILED_DEF(R_CONJ3, bank, term);

    // CONJ(CONJ(a)) -> a
    REWRITE_COMPILED_DEF(R_CONJ4, bank, term);

    // The scalar rule list.
    extern const std::vector<ualg::RewritingRule<int>> scalar_rules;

} // namespace scalar
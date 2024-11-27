#pragma once

#include "ualg.hpp"

namespace scalar {

    extern ualg::StringSymbolType symbols;

    extern const ualg::Signature<int> reserved_sig;

    //////////////// properties of the symbols

    // ADDS(a) -> a
    REWRITE_COMPILED_DEF(R_ADDSID, bank, term);
    // MULS(a) -> a
    REWRITE_COMPILED_DEF(R_MULSID, bank, term);


    //////////////// rewriting rules

    // ADDS(a 0) -> a
    REWRITE_COMPILED_DEF(R_ADDS0, bank, term);

    // MULS(a 0) -> 0
    REWRITE_COMPILED_DEF(R_MULS0, bank, term);

    // MULS(a 1) -> a
    REWRITE_COMPILED_DEF(R_MULS1, bank, term);

    // MULS(a ADDS(b c)) -> ADDS(MULS(a b) MULS(a c))
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

} // namespace scalar
#pragma once

#include "ualg.hpp"

namespace scalar {

    extern ualg::StringSymbolType symbols;

    extern ualg::Signature<int> reserved_sig;

    // ADDS(a 0) -> a
    REWRITE_COMPILED_DEF(R_ADDS0, bank, term);

    // MLTS(a 0) -> 0
    REWRITE_COMPILED_DEF(R_MLTS0, bank, term);

    // MLTS(a 1) -> a
    REWRITE_COMPILED_DEF(R_MLTS1, bank, term);

    // MLTS(a ADDS(b c)) -> ADDS(MLTS(a b) MLTS(a c))
    REWRITE_COMPILED_DEF(R_MLTS2, bank, term);

} // namespace scalar
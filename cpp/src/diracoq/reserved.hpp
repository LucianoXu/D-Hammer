#pragma once

#include "ualg.hpp"
#include <string>
#include <set>

namespace diracoq {

    extern ualg::StringSymbolType diracoq_symbols;

    extern const ualg::Signature<int> diracoq_sig;

    inline bool is_reserved(int symbol) {
        return diracoq_sig.find_name(symbol) != std::nullopt;
    }

    extern const int TYPE;
    extern const int ARROW;
    extern const int FUN;
    extern const int APPLY;

    extern const int PAIR;

    extern const int BASE,SType, KType, BType, OType, Prod;

    extern const int ZERO, ONE, ADDS, MULS, CONJ, DELTA, ZEROK, ZEROB, ZEROO, ONEO, KET, BRA, ADJ, SCR, ADD, TSR, DOT, MULK, MULB, OUTER, MULO;

    extern const std::set<int> ac_symbols;

} // namespace diracoq
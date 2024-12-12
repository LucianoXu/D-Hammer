#pragma once

#include "ualg.hpp"
#include <string>
#include <set>

namespace diracoq {

    // de Bruijn indices are denoted by $0, $1, $2, ...

    extern const int deBruijn_index_num;

    inline bool is_deBruijn_index(int symbol) {
        return symbol < deBruijn_index_num;
    }

    extern ualg::StringSymbolType diracoq_symbols;

    extern const ualg::Signature<int> diracoq_sig;

    inline bool is_reserved(int symbol) {
        return diracoq_sig.find_name(symbol) != std::nullopt;
    }

    extern const int INDEX, TYPE;
    extern const int Prod;
    extern const int BASIS, SType, KType, BType, OType, ARROW, FORALL, Set;

    extern const int PAIR, FUN, APPLY;
    
    extern const int ZERO, ONE, ADDS, MULS, CONJ, DELTA, DOT, ZEROK, ZEROB, ZEROO, ONEO, KET, BRA, ADJ, SCR, ADD, TSR, MULK, MULB, OUTER, MULO;

    extern const int USET, CATPROD, SUM;

    extern const std::set<int> a_symbols;
    extern const std::set<int> c_symbols;

} // namespace diracoq
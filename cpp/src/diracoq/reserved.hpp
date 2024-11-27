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

    extern const int BASE;
    extern const int SType;
    extern const int KType;
    extern const int BType;
    extern const int OType;

    extern const int ZERO;
    extern const int ONE;
    extern const int CONJ;
    extern const int ADDS;
    extern const int MULS;

    extern const std::set<int> ac_symbols;

} // namespace diracoq
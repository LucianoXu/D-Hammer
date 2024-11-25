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

    const auto TYPE = diracoq_sig.get_repr("Type");
    const auto ARROW = diracoq_sig.get_repr("Arrow");
    const auto FUN = diracoq_sig.get_repr("fun");
    const auto APPLY = diracoq_sig.get_repr("apply");

    const auto BASE = diracoq_sig.get_repr("Base");
    const auto SType = diracoq_sig.get_repr("SType");
    const auto KType = diracoq_sig.get_repr("KType");
    const auto BType = diracoq_sig.get_repr("BType");
    const auto OType = diracoq_sig.get_repr("OType");

} // namespace diracoq
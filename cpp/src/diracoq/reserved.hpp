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

} // namespace diracoq
#pragma once

#include "ualg.hpp"
#include <string>
#include <set>

namespace diracoq {

    extern ualg::StringSymbolType CoC_symbols;

    extern const ualg::Signature<int> CoC_sig;

    inline bool is_reserved(int symbol) {
        return CoC_sig.find_name(symbol) != std::nullopt;
    }

} // namespace diracoq
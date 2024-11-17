#include "ualg.hpp"

namespace ualg {

    Signature<int> compile_string_sig(const StringSymbolType& symbol_types) {
        std::map<std::string, int> head_mapping;
        std::map<int, SymbolType> symbol_type_mapping;

        for (const auto& [name, type] : symbol_types) {
            head_mapping[name] = head_mapping.size();
            symbol_type_mapping[head_mapping[name]] = type;
        }

        return {head_mapping, symbol_type_mapping};
    }

    StringSymbolType extend_string_symbol_list(const StringSymbolType& symbol_types_A, const StringSymbolType& symbol_types_B) {
        StringSymbolType symbol_types = symbol_types_A;
        symbol_types.insert(symbol_types.end(), symbol_types_B.begin(), symbol_types_B.end());
        return symbol_types;
    }

}
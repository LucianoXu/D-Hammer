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

}
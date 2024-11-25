#include "diracoq.hpp"

namespace diracoq {

    using namespace ualg;

    StringSymbolType diracoq_symbols = {
        {"Group", SymbolType::NORMAL},
        {"Def", SymbolType::NORMAL},
        {"Var", SymbolType::NORMAL},
        {"Check", SymbolType::NORMAL},
        {"Show", SymbolType::NORMAL},
        {"ShowAll", SymbolType::NORMAL},


        {"Type", SymbolType::NORMAL},
        {"Arrow", SymbolType::NORMAL},
        {"fun", SymbolType::NORMAL},
        {"apply", SymbolType::NORMAL},
        
        {"Base", SymbolType::NORMAL},
        {"SType", SymbolType::NORMAL},
        {"KType", SymbolType::NORMAL},
        {"BType", SymbolType::NORMAL},
        {"OType", SymbolType::NORMAL},
    };

    const Signature<int> diracoq_sig = compile_string_sig(diracoq_symbols);

} // namespace diracoq
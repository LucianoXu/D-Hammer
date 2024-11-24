#include "diracoq.hpp"

namespace diracoq {

    using namespace ualg;

    StringSymbolType CoC_symbols = {
        {"Group", SymbolType::NORMAL},
        {"Def", SymbolType::NORMAL},
        {"Var", SymbolType::NORMAL},
        {"Check", SymbolType::NORMAL},
        {"ShowAll", SymbolType::NORMAL},


        {"Type", SymbolType::NORMAL},
        {"forall", SymbolType::NORMAL},
        {"fun", SymbolType::NORMAL},
        {"apply", SymbolType::NORMAL},
        
        {"Base", SymbolType::NORMAL}
    };

    const Signature<int> CoC_sig = compile_string_sig(CoC_symbols);

} // namespace diracoq
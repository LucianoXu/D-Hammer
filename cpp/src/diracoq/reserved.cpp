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
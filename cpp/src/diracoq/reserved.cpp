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
        {"Normalize", SymbolType::NORMAL},
        {"CheckEq", SymbolType::NORMAL},


        {"Type", SymbolType::NORMAL},
        {"Arrow", SymbolType::NORMAL},
        {"fun", SymbolType::NORMAL},
        {"apply", SymbolType::NORMAL},
        
        {"Base", SymbolType::NORMAL},
        {"SType", SymbolType::NORMAL},
        {"KType", SymbolType::NORMAL},
        {"BType", SymbolType::NORMAL},
        {"OType", SymbolType::NORMAL},

        {"0", SymbolType::NORMAL},
        {"1", SymbolType::NORMAL},
        {"CONJ", SymbolType::NORMAL},
        {"ADDS", SymbolType::NORMAL},
        {"MULS", SymbolType::NORMAL}
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

    const auto ZERO = diracoq_sig.get_repr("0");
    const auto ONE = diracoq_sig.get_repr("1");
    const auto CONJ = diracoq_sig.get_repr("CONJ");
    const auto ADDS = diracoq_sig.get_repr("ADDS");
    const auto MULS = diracoq_sig.get_repr("MULS");

    const std::set<int> ac_symbols = {ADDS, MULS};


} // namespace diracoq
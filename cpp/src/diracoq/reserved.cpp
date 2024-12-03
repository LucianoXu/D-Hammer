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
        {"Prod", SymbolType::NORMAL},
        
        {"PAIR", SymbolType::NORMAL},

        {"0", SymbolType::NORMAL},
        {"1", SymbolType::NORMAL},
        {"ADDS", SymbolType::NORMAL},
        {"MULS", SymbolType::NORMAL},
        {"CONJ", SymbolType::NORMAL},
        {"DELTA", SymbolType::NORMAL},

        {"0K", SymbolType::NORMAL},
        {"0B", SymbolType::NORMAL},
        {"0O", SymbolType::NORMAL},
        {"1O", SymbolType::NORMAL},
        {"KET", SymbolType::NORMAL},
        {"BRA", SymbolType::NORMAL},
        {"ADJ", SymbolType::NORMAL},
        {"SCR", SymbolType::NORMAL},
        {"ADD", SymbolType::NORMAL},
        {"TSR", SymbolType::NORMAL},

        {"DOT", SymbolType::NORMAL},
        {"MULK", SymbolType::NORMAL},
        {"MULB", SymbolType::NORMAL},
        {"OUTER", SymbolType::NORMAL},
        {"MULO", SymbolType::NORMAL}
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
    const auto Prod = diracoq_sig.get_repr("Prod");

    const auto PAIR = diracoq_sig.get_repr("PAIR");

    const auto ZERO = diracoq_sig.get_repr("0");
    const auto ONE = diracoq_sig.get_repr("1");
    const auto ADDS = diracoq_sig.get_repr("ADDS");
    const auto MULS = diracoq_sig.get_repr("MULS");
    const auto CONJ = diracoq_sig.get_repr("CONJ");
    const auto DELTA = diracoq_sig.get_repr("DELTA");

    const auto ZEROK = diracoq_sig.get_repr("0K");
    const auto ZEROB = diracoq_sig.get_repr("0B");
    const auto ZEROO = diracoq_sig.get_repr("0O");
    const auto ONEO = diracoq_sig.get_repr("1O");
    const auto KET = diracoq_sig.get_repr("KET");
    const auto BRA = diracoq_sig.get_repr("BRA");
    const auto ADJ = diracoq_sig.get_repr("ADJ");
    const auto SCR = diracoq_sig.get_repr("SCR");
    const auto ADD = diracoq_sig.get_repr("ADD");
    const auto TSR = diracoq_sig.get_repr("TSR");

    const auto DOT = diracoq_sig.get_repr("DOT");
    const auto MULK = diracoq_sig.get_repr("MULK");
    const auto MULB = diracoq_sig.get_repr("MULB");
    const auto OUTER = diracoq_sig.get_repr("OUTER");
    const auto MULO = diracoq_sig.get_repr("MULO");

    const std::set<int> a_symbols = {ADDS, MULS, ADD};
    const std::set<int> c_symbols = {ADDS, MULS, DELTA, ADD};


} // namespace diracoq
#pragma once

#include "ualg.hpp"
#include <string>
#include <set>

namespace diracoq {

    template <class T>
    inline ualg::TermPtr<T> create_term(const T& head) {
        return std::make_shared<const ualg::Term<T>>(head);
    }

    template <class T>
    inline ualg::TermPtr<T> create_term(const T& head, ualg::ListArgs<T> args) {
        return std::make_shared<const ualg::Term<T>>(head, std::move(args));
    }

    extern const int deBruijn_index_num;

    extern std::vector<std::string> diracoq_symbols;

    extern const ualg::Signature<int> diracoq_sig;

    inline bool is_reserved(int symbol) {
        return diracoq_sig.find_name(symbol) != std::nullopt;
    }

    // the symbol for preprocessing
    extern const int COMPO, ADDG, STAR, SSUM;

    // symbols for typing
    extern const int INDEX, TYPE;
    extern const int PROD;
    extern const int QBIT, BASIS0, BASIS1;
    extern const int BASIS, STYPE, KTYPE, BTYPE, OTYPE, ARROW, FORALL, SET;

    extern const int PAIR, FUN, IDX, APPLY;
    
    extern const int ZERO, ONE, ADDS, MULS, CONJ, DELTA, ZEROK, ZEROB, ZEROO, ONEO, KET, BRA, ADJ, SCR, ADD, TSR, DOT;

    extern const int USET, CATPROD, SUM;

    extern const std::set<int> a_symbols;
    extern const std::set<int> c_symbols;
    
} // namespace diracoq
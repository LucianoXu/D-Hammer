#pragma once

#include "ualg.hpp"
#include "reserved.hpp"

namespace diracoq {

    const ualg::NormalTerm<int>* deBruijn_replace(ualg::TermBank<int>& bank, const ualg::Term<int>* term, int index, const ualg::Term<int>* t);

    /**
     * @brief Instantiate the specified bound variable with term t, in the de Bruijn expression term. Reduce the index of larger than or equal to the specified index by 1.
     * 
     * Example: instantiate(fun(fun($2 ($3 $4))), 1, x) = fun(fun($2 (x $3)))
     * 
     * @param term 
     * @param t 
     * @return const ualg::Term<int>* 
     */
    const ualg::NormalTerm<int>* instantiate(ualg::TermBank<int>& bank, const ualg::Term<int>* term, int index, const ualg::Term<int>* t);


    /**
     * @brief Adjust the de Bruijn index in the term (greater or equal to the specified index) by the specified adjustment.
     * 
     * @param bank 
     * @param term 
     * @param index 
     * @param adjustment 
     * @return const ualg::NormalTerm<int>* 
     */
    const ualg::NormalTerm<int>* deBruijn_adjust(ualg::TermBank<int>& bank, const ualg::Term<int>* term, int index, int adjustment);


    /**
     * @brief Check whether the given index is free in the term.
     * 
     * @param index 
     * @param term 
     * @return true 
     * @return false 
     */
    bool deBruijn_index_free_in(int index, const ualg::Term<int>* term);

    /**
     * @brief Swap the de Bruijn index i and j in the term.
     * 
     * @param bank 
     * @param term 
     * @return const ualg::NormalTerm<int>* 
     */
    const ualg::NormalTerm<int>* deBruijn_swap(ualg::TermBank<int>& bank, const ualg::Term<int>* term, int i, int j);
} // namespace diracoq
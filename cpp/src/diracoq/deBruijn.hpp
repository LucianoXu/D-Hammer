#pragma once

#include "ualg.hpp"
#include "reserved.hpp"

namespace diracoq {
    /**
     * @brief Instantiate the first bound variable with term t, in the de Bruijn expression term.
     * 
     * Example: instantiate(fun(fun($2)), x) = fun(fun(x))
     * 
     * @param term 
     * @param t 
     * @return const ualg::Term<int>* 
     */
    const ualg::NormalTerm<int>* instantiate(ualg::TermBank<int>& bank, const ualg::NormalTerm<int>* term, const ualg::NormalTerm<int>* t);
} // namespace diracoq
#pragma once

#include <string>
#include <set>
#include "term.hpp"
#include "termbank.hpp"

namespace ualg {

    ///////////////////////////////////////////////////////////////
    // AC-theory by normal terms + sorting

    /**
     * @brief Flatten the term by the AC symbols.
     * 
     * @tparam T 
     * @param term 
     * @param bank 
     * @param c_symbols 
     * @return const Term<T>* 
     */
    template <class T>
    const Term<T>* flatten(const Term<T>* term, TermBank<T>& bank, const std::set<T>& c_symbols) {

        // If the term is atomic or not an AC symbol, return the term
        if (term->is_atomic() || c_symbols.find(term->get_head()) == c_symbols.end()) return term;

        const ListArgs<T>& args = term->get_args();
        ListArgs<T> new_args;

        for (const auto& arg : args) {
            // flatten the subterm first
            const auto new_arg = flatten<T>(arg, bank, c_symbols);
            if (new_arg->get_head() == term->get_head()) {
                auto& arg_args = new_arg->get_args();

                new_args.insert(new_args.end(), arg_args.begin(), arg_args.end());

            } else {
                new_args.push_back(new_arg);
            }
        }

        return bank.get_term(term->get_head(), std::move(new_args));
    }
    
    /**
     * @brief sort the commutative terms in the term.
     * 
     * @tparam T 
     * @param term 
     * @param bank 
     * @param c_symbols 
     * @return const Term<T>* 
     */
    template <class T>
    const Term<T>* sort_C_terms(const Term<T>* term, TermBank<T>& bank, const std::set<T>& c_symbols) {

        if (term->get_args().size() == 0) return term;

        const ListArgs<T>& args = term->get_args();
        
        // sort within the arguments
        ListArgs<T> res_subterm_sort;
        for (unsigned i = 0; i < args.size(); ++i) {
            auto sorted_arg = sort_C_terms(
                args[i],
                bank, c_symbols
            );
            res_subterm_sort.push_back(sorted_arg);
        }

        // sort the arguments for AC symbols
        if (c_symbols.find(term->get_head()) != c_symbols.end()) {
            std::sort(res_subterm_sort.begin(), res_subterm_sort.end(), 
                [](const auto& a, const auto& b) {
                    return a < b;
                }
            );
        }

        return  bank.get_term(term->get_head(), std::move(res_subterm_sort));
    }


    /**
     * @brief Check whether the two terms are equivalent under the C theory.
     * 
     * @tparam T 
     * @param termA 
     * @param termB 
     * @return bool
     */
    template <class T>
    bool check_C_eq(const Term<T>* termA, const Term<T>* termB, TermBank<T>& bank, const std::set<T>& c_symbols) {

        // sort the two terms first
        auto sortedA = sort_C_terms(termA, bank, c_symbols);
        auto sortedB = sort_C_terms(termB, bank, c_symbols);

        return sortedA == sortedB;
    }
}
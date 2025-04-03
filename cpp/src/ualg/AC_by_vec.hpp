#pragma once

#include <algorithm>
#include <string>
#include <set>
#include "term.hpp"

namespace ualg {

    ///////////////////////////////////////////////////////////////
    // AC-theory by normal terms + sorting

    /**
     * @brief Flatten the term by the AC symbols.
     * 
     * @tparam T 
     * @param term 
     * @param c_symbols 
     * @return const Term<T>* 
     */
    template <class T>
    TermPtr<T> flatten(TermPtr<T> term, const std::set<T>& c_symbols) {

        // If the term is atomic or not an AC symbol, return the term
        if (term->is_atomic() || c_symbols.find(term->get_head()) == c_symbols.end()) return term;

        const ListArgs<T>& args = term->get_args();
        ListArgs<T> new_args;

        for (const auto& arg : args) {
            // flatten the subterm first
            auto new_arg = flatten<T>(arg, c_symbols);
            if (new_arg->get_head() == term->get_head()) {
                auto& arg_args = new_arg->get_args();

                new_args.insert(new_args.end(), arg_args.begin(), arg_args.end());

            } else {
                new_args.push_back(new_arg);
            }
        }

        return std::make_shared<Term<T>>(term->get_head(), std::move(new_args));
    }
    
    /**
     * @brief sort the commutative terms in the term.
     * 
     * @tparam T 
     * @param term
     * @param c_symbols 
     * @param comp 
     * @return const Term<T>* 
     */
    template <class T, class Compare>
    TermPtr<T> sort_C_terms(TermPtr<T> term, const std::set<T>& c_symbols, Compare comp) {

        if (term->get_args().size() == 0) return term;

        const ListArgs<T>& args = term->get_args();
        
        // sort within the arguments
        ListArgs<T> res_subterm_sort;
        for (unsigned i = 0; i < args.size(); ++i) {
            auto sorted_arg = sort_C_terms(
                args[i], c_symbols, comp
            );
            res_subterm_sort.push_back(sorted_arg);
        }

        // sort the arguments for AC symbols
        if (c_symbols.find(term->get_head()) != c_symbols.end()) {
            std::sort(res_subterm_sort.begin(), res_subterm_sort.end(), comp);
        }

        return  std::make_shared<Term<T>>(term->get_head(), std::move(res_subterm_sort));
    }

    /**
     * @brief The standard comparator for sort_C_terms.
     * 
     * @tparam T 
     * @param a 
     * @param b 
     * @return true 
     * @return false 
     */
    template <class T>
    bool std_comp(TermPtr<T> a, TermPtr<T> b) {
        return *a < *b;
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
    bool check_C_eq(TermPtr<T> termA, TermPtr<T> termB, const std::set<T>& c_symbols) {

        // sort the two terms first
        auto sortedA = sort_C_terms(termA, c_symbols, std_comp<T>);
        auto sortedB = sort_C_terms(termB, c_symbols, std_comp<T>);

        return *sortedA == *sortedB;
    }
}
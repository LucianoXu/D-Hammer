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
     * @brief The instruction to transform a term to another under the C theory.
     * 
     */
    struct CProofInstruct {
        using PermutationSeq = std::vector<std::pair<unsigned, CProofInstruct>>;
        
        PermutationSeq ls;

        CProofInstruct() {}
        CProofInstruct(const PermutationSeq& ls) {
            this->ls = ls;
        }

        // assign operator
        CProofInstruct& operator = (const CProofInstruct& other) {
            this->ls = other.ls;
            return *this;
        }

        // move assign operator
        CProofInstruct& operator = (CProofInstruct&& other) {
            this->ls = std::move(other.ls);
            return *this;
        }

        // copy constructor
        CProofInstruct(const CProofInstruct& other) {
            this->ls = other.ls;
        }

        // move constructor
        CProofInstruct(CProofInstruct&& other) {
            this->ls = std::move(other.ls);
        }

        inline bool E() const {
            return this->ls.size() == 0;
        }

        std::string to_string() const;
        CProofInstruct compose(const CProofInstruct& other) const;
        CProofInstruct inverse() const;
    };

    template <class T>
    const Term<T>* apply_CInstruct(const Term<T>* term, const CProofInstruct& instruct, TermBank<T>& bank) {
        if (instruct.E()) return term;

        const ListArgs<T>& args = term->get_args();
        ListArgs<T> new_args;

        for (int i = 0; i < args.size(); ++i) {
            const auto& idx = instruct.ls[i].first;
            const auto& sub_instruct = instruct.ls[idx].second;
            
            new_args.push_back(apply_CInstruct(args[idx], sub_instruct, bank));
        }

        return bank.get_term(term->get_head(), std::move(new_args));
    } 

    template <class T>
    std::pair<const Term<T>*, CProofInstruct> sort_CInstruct(const Term<T>* term, TermBank<T>& bank, const std::set<T>& c_symbols) {

        if (term->get_args().size() == 0) return {term, CProofInstruct{}};

        const ListArgs<T>& args = term->get_args();
        
        // sort within the arguments
        std::vector<std::pair<const Term<T>*, unsigned>> res_subterm_sort;
        std::vector<CProofInstruct> subterm_instructs;
        for (unsigned i = 0; i < args.size(); ++i) {
            auto [sorted_arg, instruct] = sort_CInstruct(
                args[i],
                bank, c_symbols
            );
            subterm_instructs.push_back(instruct);
            res_subterm_sort.push_back({sorted_arg, i});
        }

        // sort the arguments for AC symbols
        if (c_symbols.find(term->get_head()) != c_symbols.end()) {
            std::sort(res_subterm_sort.begin(), res_subterm_sort.end(), 
                [](const auto& a, const auto& b) {
                    return a.first < b.first;
                }
            );
        }

        ListArgs<T> new_args;
        CProofInstruct::PermutationSeq seq;
        for (int i = 0; i < res_subterm_sort.size(); ++i) {
            const auto& [arg, idx] = res_subterm_sort[i];
            new_args.push_back(arg);
            seq.push_back({idx, std::move(subterm_instructs[i])});
        }

        auto res = bank.get_term(term->get_head(), std::move(new_args));
        
        return {res, CProofInstruct{seq}};
    }


    /**
     * @brief Check whether the two terms are equivalent under the C theory.
     * 
     * @tparam T 
     * @param termA 
     * @param termB 
     * @return std::optional<CProofInstruct>. If the two terms are equivalent, return the permutation to transform A to B. If not equivalent, return std::nullopt.
     */
    template <class T>
    std::optional<CProofInstruct> check_C_eq(const Term<T>* termA, const Term<T>* termB, TermBank<T>& bank, const std::set<T>& c_symbols) {

        // sort the two terms first
        auto [sortedA, instructA] = sort_CInstruct(termA, bank, c_symbols);
        auto [sortedB, instructB] = sort_CInstruct(termB, bank, c_symbols);

        if (sortedA == sortedB) {
            return instructA.compose(instructB.inverse());
        }

        return std::nullopt;
    }
}
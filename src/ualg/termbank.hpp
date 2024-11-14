#pragma once

#include "term.hpp"

namespace ualg {

    using TermMapping = boost::unordered_map<const Term*, const Term*>;

    // The term bank. The signature of the term bank is fixed.
    class TermBank {
    private:
        boost::unordered_set<NormalTerm> normal_terms;

    public:
        TermBank() {}

        unsigned int size() const;

        // Create a term from bank.
        // The properties of the symbols will be processed here.
        // NOTICE: make sure that all the subterms are already in the bank.
        const NormalTerm* get_normal_term(const std::string& head, std::vector<const Term*>&& args);

        // Create a term from bank.
        // The properties of the symbols will be processed here.
        // NOTICE: make sure that all the subterms are already in the bank.
        const NormalTerm* get_normal_term(const std::string& head, const std::vector<const Term*>& args);

        // Inductively construct a term in the bank. Check every subterms.
        // Inner terms are reconstructed first.
        const Term* construct_term(const Term& term);

    private:
        // Replace all the occurrences of p_old_term with p_new_term in the term.
        // mapping: the mapping from the old terms to the new terms
        const Term* _replace_term(
            const Term* term, 
            const TermMapping& mapping,
            TermMapping& cache);

    public:

        // Replace all the occurrences of p_old_term with p_new_term in the term.
        // mapping: the mapping from the old terms to the new terms
        // NOTICE: all terms should be in the bank.
        const Term* replace_term(
            const Term* term, 
            const TermMapping& mapping);
    };


} // namespace ualg
#pragma once

#include "reserved.hpp"
#include "ualg.hpp"
#include <boost/unordered_map.hpp>

namespace diracoq {

    struct Definition {
        std::optional<const ualg::Term<int>*> def;
        const ualg::Term<int>* type;

        inline bool is_def() const {
            return def.has_value();
        }
    };

    /** 
     * @brief The kernel of the proof assistant.
     * 
     * The kernel of the proof assistant preserves the TermBank, the environment and the context.
     * The environment and context are guaranteed to be always well-formed.
     */
    class Kernel {
    protected:
        ualg::TermBank<int> bank;
        ualg::Signature<int> sig;
        std::vector<std::pair<int, Definition>> env;
        std::vector<std::pair<int, Definition>> context;

    protected:
        /**
         * @brief Find the assumption/definition of the symbol in the context.
         * 
         * @param symbol 
         * @return std::optional<Definition> If the symbol is not found, return `std::nullopt`.
         */
        std::optional<Definition> find_in_context(int symbol);


        /**
         * @brief Find the assumption/definition of the symbol in the env.
         * 
         * @param symbol 
         * @return std::optional<Definition> If the symbol is not found, return `std::nullopt`.
         */
        std::optional<Definition> find_in_env(int symbol);
    
    public:
        Kernel() : sig(CoC_sig) {}

        inline int register_symbol(const std::string& name) {
            return sig.register_symbol(name);
        }

        inline ualg::TermBank<int>& get_bank() {
            return bank;
        }

        inline const ualg::Signature<int>& get_sig() {
            return sig;
        }

        /**
         * @brief Parse the code and return the term.
         * 
         * Using the preserved signature and the term bank.
         * 
         * @param code 
         * @return const Term<int>* 
         */
        const ualg::Term<int>* parse(const std::string& code);


        /**
         * @brief Parse the code and return the term.
         * 
         * @param ast 
         * @return const ualg::Term<int>* 
         */
        const ualg::Term<int>* parse(const astparser::AST& ast);

        /**
         * @brief Transform the term to a string.
         * 
         * @param term 
         * @return string 
         */
        std::string term_to_string(const ualg::Term<int>* term) const;

        std::string env_to_string() const;

        std::string context_to_string() const;

        /**
         * @brief Checks if the term is a sort.
         * 
         * @param term a well-typed term.
         * @return true 
         * @return false 
         */
        bool is_sort(const ualg::Term<int>* term);

        /**
         * @brief Return the maximum type of the two types.
         * 
         * @param type1 A well-typed term.
         * @param type2 A well-typed term.
         * @return const Term<int>* 
         */
        const ualg::Term<int>* max_Type(const ualg::Term<int>* type1, const ualg::Term<int>* type2);

        /**
         * @brief Calculate and return the least type of the term.
         * 
         * Raise an error if the term is not well-typed.
         * 
         * @param term 
         * @return const ualg::Term<int>* , the least type of the term.
         */
        const ualg::Term<int>* calc_type(const ualg::Term<int>* term);

        /**
         * @brief Whether the two terms are equal w.r.t. the conversions and reductions in CoC.
         * 
         * @param term1 A well-typed term.
         * @param term2 A well-typed term.
         * @return true 
         * @return false 
         */
        bool coc_eq(const ualg::Term<int>* term1, const ualg::Term<int>* term2);


        /**
         * @brief Check whether the type1 is a subtype of type2.
         * 
         * @param type1 A well-typed term.
         * @param type2 A well-typed term.
         * @return true 
         * @return false 
         */
        bool subtyping(const ualg::Term<int>* type1, const ualg::Term<int>* type2);

        /**
         * @brief Check if the term is well-formed and well-typed.
         * 
         * @param term 
         * @return true 
         * @return false 
         */
        bool type_check(const ualg::Term<int>* term, const ualg::Term<int>* type);


        //////////////////////////////////////////////////////////////////////////
        // methods to modify the well-formed kernel
        
        /**
         * @brief Make an assumption in the context.
         * 
         * @param symbol 
         * @param type 
         */
        void local_assum(int symbol, const ualg::Term<int>* type);

        /**
         * @brief Make a definition in the context.
         * 
         * @param symbol 
         * @param def 
         * @param type if not provided, the type will be calculated.
         */
        void local_def(int symbol, const ualg::Term<int>* def, std::optional<const ualg::Term<int>*> type = std::nullopt);

        /**
         * @brief Remove the last assumption/definition in the context.
         * 
         */
        void local_pop();

        /**
         * @brief Clean all the assumptions/definitions in the context.
         * 
         */
        void local_clear();

        /**
         * @brief Make an assumption in the environment.
         * 
         * @param symbol 
         * @param type 
         */
        void global_assum(int symbol, const ualg::Term<int>* type);

        /**
         * @brief Make a definition in the environment.
         * 
         * @param symbol 
         * @param def 
         * @param type if not provided, the type will be calculated.
         */
        void gloabl_def(int symbol, const ualg::Term<int>* def, std::optional<const ualg::Term<int>*> type = std::nullopt);
    };
} // namespace diracoq
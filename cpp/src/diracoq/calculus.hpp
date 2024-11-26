#pragma once

#include "reserved.hpp"
#include "ualg.hpp"
#include <boost/unordered_map.hpp>

namespace diracoq {

    struct Declaration {
        std::optional<const ualg::Term<int>*> def;
        const ualg::Term<int>* type;

        inline bool is_def() const {
            return def.has_value();
        }
    };

    /** 
     * @brief The kernel of the proof assistant.
     * 
     * The kernel of the proof assistant preserves the TermBank and the environment.
     * The environment is guaranteed to be always well-formed.
     */
    class Kernel {
    protected:
        ualg::TermBank<int> bank;
        ualg::Signature<int> sig;
        std::vector<std::pair<int, Declaration>> env;

    public:
        Kernel() : sig(diracoq_sig) {}

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
         * @brief Find the assumption/definition of the symbol in the env.
         * 
         * @param symbol 
         * @return std::optional<Declaration> If the symbol is not found, return `std::nullopt`.
         */
        std::optional<Declaration> find_in_env(int symbol);

        std::string dec_to_string(const std::string& name, const Declaration& dec) const;
    
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

        /**
         * @brief Checks if the term is a type.
         * 
         * @param term a well-typed term.
         * @return true 
         * @return false 
         */
        bool is_type(const ualg::Term<int>* term);

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
         * @brief Check if the term is well-formed and well-typed.
         * 
         * @param term 
         * @return true 
         * @return false 
         */
        bool type_check(const ualg::Term<int>* term, const ualg::Term<int>* type);

        /**
         * @brief Make an assumption in the environment.
         * 
         * @param symbol 
         * @param type 
         */
        void assum(int symbol, const ualg::Term<int>* type);

        /**
         * @brief Make a definition in the environment.
         * 
         * @param symbol 
         * @param def 
         * @param type if not provided, the type will be calculated.
         */
        void def(int symbol, const ualg::Term<int>* def, std::optional<const ualg::Term<int>*> type = std::nullopt);

        /**
         * @brief Pop the last assumption/definition in the environment.
         * 
         */
        void env_pop();
    };
} // namespace diracoq
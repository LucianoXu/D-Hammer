#pragma once

#include "symbols.hpp"
#include "ualg.hpp"
#include <boost/unordered_map.hpp>

namespace diracoq {

    struct Declaration {
        std::optional<const ualg::Term<int>*> def;
        const ualg::Term<int>* type;

        inline bool is_def() const {
            return def.has_value();
        }

        inline bool is_index_assum() const {
            return !is_def() && type->get_head() == INDEX;
        }

        inline bool is_type_assum() const {
            return !is_def() && type->get_head() == TYPE;
        }

        inline bool is_term() const {
            return type->get_head() != INDEX && type->get_head() != TYPE;
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
        std::vector<std::pair<int, Declaration>> ctx;

        inline void arg_number_check(const ualg::ListArgs<int>& args, int num) {
            if (args.size() != num) {
                throw std::runtime_error("Typing error: the term is not well-typed, because the argument number is not " + std::to_string(num) + ".");
            }
        }

    public:
        Kernel() : sig(diracoq_sig) {}

        // copy constructor
        Kernel(const Kernel& other) : bank(other.bank), sig(other.sig), env(other.env), ctx(other.ctx) {}

        inline int register_symbol(const std::string& name) {
            return sig.register_symbol(name);
        }

        inline ualg::TermBank<int>& get_bank() {
            return bank;
        }

        inline ualg::Signature<int>& get_sig() {
            return sig;
        }

        /**
         * @brief Find the assumption/definition of the symbol in the env and context, following the shadowing principle.
         * 
         * @param symbol 
         * @return std::optional<Declaration> If the symbol is not found, return `std::nullopt`.
         */
        std::optional<Declaration> find_dec(int symbol);

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
         * @brief Checks if the expression is an index.
         * 
         * @param term a well-typed term.
         * @return true 
         * @return false 
         */
        bool is_index(const ualg::Term<int>* term);

        /**
         * @brief Checks if the expression is a type.
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
         * @brief Check whether two terms are equivalent under the reduction rules and alpha equivalence.
         * 
         * @param termA 
         * @param termB 
         * @return true 
         * @return false 
         */
        bool is_judgemental_eq(const ualg::Term<int>* termA, const ualg::Term<int>* termB);


        /**
         * @brief Check if the term is well-formed and well-typed. Depending on is_judgemental_eq.
         * 
         * @param term 
         * @return true 
         * @return false 
         */
        bool type_check(const ualg::Term<int>* term, const ualg::Term<int>* type) {
            return is_judgemental_eq(calc_type(term), type);
        }

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

        void context_push(int symbol, const ualg::Term<int>* type);

        void context_pop();
    };
} // namespace diracoq
#pragma once

#include "symbols.hpp"
#include "ualg.hpp"
#include <boost/unordered_map.hpp>

namespace diracoq {

    struct Declaration {
        std::optional<ualg::TermPtr<int>> def;
        ualg::TermPtr<int> type;

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
        Kernel(const Kernel& other) : sig(other.sig), env(other.env), ctx(other.ctx) {}

        // move constructor
        Kernel(Kernel&& other) : sig(std::move(other.sig)), env(std::move(other.env)), ctx(std::move(other.ctx)) {}

        inline int register_symbol(const std::string& name) {
            return sig.register_symbol(name);
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
        ualg::TermPtr<int> parse(const std::string& code);


        /**
         * @brief Parse the code and return the term.
         * 
         * @param ast 
         * @return const ualg::Term<int>* 
         */
        ualg::TermPtr<int> parse(const astparser::AST& ast);

        /**
         * @brief Transform the term to a string.
         * 
         * @param term 
         * @return string 
         */
        std::string term_to_string(ualg::TermPtr<int> term) const;

        std::string env_to_string() const;

        /**
         * @brief Checks if the expression is an index.
         * 
         * @param term a well-typed term.
         * @return true 
         * @return false 
         */
        bool is_index(ualg::TermPtr<int> term);

        /**
         * @brief Checks if the expression is a type.
         * 
         * @param term a well-typed term.
         * @return true 
         * @return false 
         */
        bool is_type(ualg::TermPtr<int> term);

        /**
         * @brief Calculate and return the least type of the term.
         * 
         * Raise an error if the term is not well-typed.
         * 
         * @param term 
         * @return const ualg::Term<int>* , the least type of the term.
         */
        ualg::TermPtr<int> calc_type(ualg::TermPtr<int> term);


        /**
         * @brief Check whether two terms are equivalent under the reduction rules and alpha equivalence.
         * 
         * @param termA 
         * @param termB 
         * @return true 
         * @return false 
         */
        bool is_judgemental_eq(ualg::TermPtr<int> termA, ualg::TermPtr<int> termB);


        /**
         * @brief Check if the term is well-formed and well-typed. Depending on is_judgemental_eq.
         * 
         * @param term 
         * @return true 
         * @return false 
         */
        bool type_check(ualg::TermPtr<int> term, ualg::TermPtr<int> type) {
            return is_judgemental_eq(calc_type(term), type);
        }

        /**
         * @brief Make an assumption in the environment.
         * 
         * @param symbol 
         * @param type 
         */
        void assum(int symbol, ualg::TermPtr<int> type);

        /**
         * @brief Make a definition in the environment.
         * 
         * @param symbol 
         * @param def 
         * @param type if not provided, the type will be calculated.
         */
        void def(int symbol, ualg::TermPtr<int> def, std::optional<ualg::TermPtr<int>> type = std::nullopt);

        /**
         * @brief Pop the last assumption/definition in the environment.
         * 
         */
        void env_pop();

        void context_push(int symbol, ualg::TermPtr<int> type);

        void context_pop();
    };
} // namespace diracoq
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
        unsigned long long unique_var_id = 0;
        ualg::TermBank<int> bank;
        ualg::Signature<int> sig;
        std::vector<std::pair<int, Declaration>> env;
        std::vector<std::vector<const ualg::Term<int>*>> ctx_stack;

        inline void arg_number_check(const ualg::ListArgs<int>& args, int num) {
            if (args.size() != num) {
                throw std::runtime_error("Typing error: the term is not well-typed, because the argument number is not " + std::to_string(num) + ".");
            }
        }

        // inline void arg_type_check(const ualg::Term<int>* term, const ualg::Term<int>* type) {
        //     if (!type_check(term, type)) {
        //         throw std::runtime_error("Typing error: the term '" + sig.term_to_string(term) + "' is not well-typed with the type '" + sig.term_to_string(type) + "'.");
        //     }
        // }

    public:
        Kernel() : sig(diracoq_sig), ctx_stack({{}}) {}

        inline int register_symbol(const std::string& name) {
            return sig.register_symbol(name);
        }

        inline const ualg::Term<int>* unique_var() {
            return parse("@" + std::to_string(unique_var_id++));
        }

        inline ualg::TermBank<int>& get_bank() {
            return bank;
        }

        inline const std::vector<const ualg::Term<int>*>& get_ctx() const{
            return ctx_stack.back();
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
         * @brief Checks if the expression is a index.
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
         * @brief Check if the term is well-formed and well-typed.
         * 
         * @param term 
         * @return true 
         * @return false 
         */
        bool type_check(const ualg::Term<int>* term, const ualg::Term<int>* type) {
            return calc_type(term) == type;
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

        void context_push(const ualg::Term<int>* term);

        void context_pop();


        inline const ualg::Term<int>* context_at_i(int i) const {
            auto& ctx = ctx_stack.back();
            if (i >= ctx.size()) {
                throw std::runtime_error("Bound variable index out of range: the context has only " + std::to_string(ctx.size()) + " elements, but the bound variable is $" + std::to_string(i) + ".");
            }
            return ctx[ctx.size() - 1 - i];
        }
    };
} // namespace diracoq
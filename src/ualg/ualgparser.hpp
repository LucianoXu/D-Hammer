#include "term.hpp"
#include "termbank.hpp"
#include "astparser.hpp"

// Transform code, or AST, to terms in the bank

namespace ualg {

    enum SymbolType {
        NORMAL,
        C,
        AC
    };

    template <class T>
    struct Signature {
        std::map<T, std::string> head_naming;
        std::map<std::string, T> head_mapping;
        std::map<T, SymbolType> symbol_types;

        Signature(
            std::map<std::string, T> head_mapping,
            std::map<T, SymbolType> symbol_types
        ) : head_mapping(head_mapping), symbol_types(symbol_types) {
            for (const auto& [name, head] : head_mapping) {
                head_naming[head] = name;
            }
        }

        std::string term_to_string(const Term<T>* term) const {
            return term->to_string(std::make_shared<const std::map<T, std::string>>(head_naming));
        }
    };

    template <class T>
    const Term<T>* ast2term(const Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast);

    template <class T>
    const Term<T>* parse(const Signature<T>& sig, TermBank<T>& bank, const std::string& code);


    //////////////////////////////////////////////////////////
    // Helper function

    /**
     * @brief Complie the Signature from a mapping of symbol names to symbol types.
     * 
     * @param symbol_types 
     * @return Signature<int> 
     */
    Signature<int> compile_string_sig(const std::map<std::string, SymbolType>& symbol_types);

    //////////////////////////////////////////////////////////
    // Implementation

    template <class T>
    const Term<T>* ast2term(const Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast) {
        auto head_find = sig.head_mapping.find(ast.head);
        if (head_find == sig.head_mapping.end()) {
            throw std::runtime_error("Unknown symbol: " + ast.head);
        }
        auto head = head_find->second;

        auto symbol_type_find = sig.symbol_types.find(head);
        if (symbol_type_find == sig.symbol_types.end()) {
            throw std::runtime_error("Unknown symbol: " + ast.head);
        }
        auto symbol_type = symbol_type_find->second;

        switch (symbol_type) {

        case SymbolType::NORMAL:
            if (ast.children.size() == 0) {
                return bank.get_normal_term(head, {});
            }
            else {
                std::vector<const Term<T>*> args;
                for (const auto& child : ast.children) {
                    args.push_back(ast2term(sig, bank, child));
                }
                return bank.get_normal_term(head, std::move(args));
            }
            break;

        case SymbolType::C:
            if (ast.children.size() == 0) {
                return bank.get_c_term(head, {});
            }
            else {
                TermCountMappping<T> args;
                for (const auto& child : ast.children) {
                    const Term<T>* child_term = ast2term(sig, bank, child);
                    update_TermCountMapping(args, child_term, 1);
                }
                return bank.get_c_term(head, std::move(args));
            }
            break;

        case SymbolType::AC:
            if (ast.children.size() == 0) {
                return bank.get_ac_term(head, {});
            }
            else {
                TermCountMappping<T> args;
                for (const auto& child : ast.children) {
                    const Term<T>* child_term = ast2term(sig, bank, child);
                    update_TermCountMapping(args, child_term, 1);
                }
                return bank.get_ac_term(head, std::move(args));
            }
            break;

        default:
            throw std::runtime_error("Unknown symbol type.");
        }
    }

    template <class T>
    const Term<T>* parse(const Signature<T>& sig, TermBank<T>& bank, const std::string& code) {
        auto ast = astparser::parse(code);
        return ast2term(sig, bank, ast);
    }

} // namespace ualg
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
        std::optional<T> (*head_mapping)(const std::string&);
        std::map<T, SymbolType> symbol_types;
    };

    std::optional<std::string> string_head_mapping(const std::string& head);

    template <class T>
    const Term<T>* ast2term(const Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast);

    template <class T>
    const Term<T>* parse(const Signature<T>& sig, TermBank<T>& bank, const std::string& code);

    //////////////////////////////////////////////////////////
    // Implementation

    template <class T>
    const Term<T>* ast2term(const Signature<T>& sig, TermBank<T>& bank, const astparser::AST& ast) {
        auto head = sig.head_mapping(ast.head);
        if (!head.has_value() || sig.symbol_types.find(head.value()) == sig.symbol_types.end()) {
            throw std::runtime_error("Unknown symbol: " + data_to_string(ast.head));
        }
        
        auto head_value = head.value();

        switch (sig.symbol_types.at(head_value)) {

        case SymbolType::NORMAL:
            if (ast.children.size() == 0) {
                return bank.get_normal_term(head_value, {});
            }
            else {
                std::vector<const Term<T>*> args;
                for (const auto& child : ast.children) {
                    args.push_back(ast2term(sig, bank, child));
                }
                return bank.get_normal_term(head_value, std::move(args));
            }
            break;

        case SymbolType::C:
            if (ast.children.size() == 0) {
                return bank.get_c_term(head_value, {});
            }
            else {
                TermCountMappping<T> args;
                for (const auto& child : ast.children) {
                    const Term<T>* child_term = ast2term(sig, bank, child);
                    update_TermCountMapping(args, child_term, 1);
                }
                return bank.get_c_term(head_value, std::move(args));
            }
            break;

        case SymbolType::AC:
            if (ast.children.size() == 0) {
                return bank.get_ac_term(head_value, {});
            }
            else {
                TermCountMappping<T> args;
                for (const auto& child : ast.children) {
                    const Term<T>* child_term = ast2term(sig, bank, child);
                    update_TermCountMapping(args, child_term, 1);
                }
                return bank.get_ac_term(head_value, std::move(args));
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
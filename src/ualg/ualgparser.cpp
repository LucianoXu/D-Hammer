#include "ualg.hpp"

namespace ualg {

    const Term<std::string>* ast2term(const Signature& sig, TermBank<std::string>& bank, const astparser::AST& ast) {
        if (sig.find(ast.head) == sig.end()) {
            throw std::runtime_error("Unknown symbol: " + ast.head);
        }

        switch (sig.at(ast.head)) {

        case SymbolType::NORMAL:
            if (ast.children.size() == 0) {
                return bank.get_normal_term(ast.head, {});
            }
            else {
                std::vector<const Term<std::string>*> args;
                for (const auto& child : ast.children) {
                    args.push_back(ast2term(sig, bank, child));
                }
                return bank.get_normal_term(ast.head, std::move(args));
            }
            break;

        case SymbolType::C:
            if (ast.children.size() == 0) {
                return bank.get_c_term(ast.head, {});
            }
            else {
                TermCountMappping<std::string> args;
                for (const auto& child : ast.children) {
                    const Term<std::string>* child_term = ast2term(sig, bank, child);
                    update_TermCountMapping(args, child_term, 1);
                }
                return bank.get_c_term(ast.head, std::move(args));
            }
            break;

        case SymbolType::AC:
            if (ast.children.size() == 0) {
                return bank.get_ac_term(ast.head, {});
            }
            else {
                TermCountMappping<std::string> args;
                for (const auto& child : ast.children) {
                    const Term<std::string>* child_term = ast2term(sig, bank, child);
                    update_TermCountMapping(args, child_term, 1);
                }
                return bank.get_ac_term(ast.head, std::move(args));
            }
            break;

        default:
            throw std::runtime_error("Unknown symbol type.");
        }
    }

    const Term<std::string>* parse(const Signature& sig, TermBank<std::string>& bank, const std::string& code) {
        auto ast = astparser::parse(code);
        return ast2term(sig, bank, ast);
    }
}
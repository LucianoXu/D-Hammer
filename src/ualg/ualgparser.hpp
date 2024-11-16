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

    using Signature = std::map<std::string, SymbolType>;

    const Term<std::string>* ast2term(const Signature& sig, TermBank<std::string>& bank, const astparser::AST& ast);

    const Term<std::string>* parse(const Signature& sig, TermBank<std::string>& bank, const std::string& code);

} // namespace ualg
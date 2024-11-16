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

    const Term* ast2term(const Signature& sig, TermBank& bank, const astparser::AST& ast);

    const Term* parse(const Signature& sig, TermBank& bank, const std::string& code);

} // namespace ualg
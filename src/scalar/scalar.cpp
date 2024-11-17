#include "scalar.hpp"

/*

The scalar module.

Signature: 
    - Constants: 0, 1
    - Unary Symbols: ADJS
    - AC Symbols: ADDS, MLTS

*/

namespace scalar {

    using namespace ualg;

    StringSymbolType symbols = {
        {"0", SymbolType::NORMAL},
        {"1", SymbolType::NORMAL},
        {"ADJS", SymbolType::NORMAL},
        {"ADDS", SymbolType::AC},
        {"MLTS", SymbolType::AC}
    };

    Signature<int> reserved_sig = compile_string_sig(symbols);

    int ZERO = reserved_sig.head_mapping["0"];
    int ONE = reserved_sig.head_mapping["1"];
    int ADJS = reserved_sig.head_mapping["ADJS"];
    int ADDS = reserved_sig.head_mapping["ADDS"];
    int MLTS = reserved_sig.head_mapping["MLTS"];

    /////////////////////////////////////////////////////////////////////////
    // Rewriting Rules

    // ADDS(a 0) -> a
    REWRITE_COMPILED_DEF(R_ADDS0, bank, term) {
        auto zero_term = bank.get_normal_term(ZERO, {});

        if (term->get_head() == ADDS) {
            const ACTerm<int>& ac_term = static_cast<const ACTerm<int>&>(*term);
            if (ac_term.get_args().find(zero_term) != ac_term.get_args().end()) {
                auto new_args = TermCountMapping<int>(ac_term.get_args());
                new_args.erase(zero_term);

                return ONE_IDENTITY_REDUCE(bank, ADDS, std::move(new_args));
            }
        }

        return std::nullopt;
    }

    // MLTS(a 0) -> 0
    REWRITE_COMPILED_DEF(R_MLTS0, bank, term) {
        auto zero_term = bank.get_normal_term(ZERO, {});

        if (term->get_head() == MLTS) {
            const ACTerm<int>& ac_term = static_cast<const ACTerm<int>&>(*term);
            if (ac_term.get_args().find(zero_term) != ac_term.get_args().end()) {
                return zero_term;
            }
        }

        return std::nullopt;
    }

    // MLTS(a 1) -> a
    REWRITE_COMPILED_DEF(R_MLTS1, bank, term) {
        auto one_term = bank.get_normal_term(ONE, {});

        if (term->get_head() == MLTS) {
            const ACTerm<int>& ac_term = static_cast<const ACTerm<int>&>(*term);
            if (ac_term.get_args().find(one_term) != ac_term.get_args().end()) {
                auto new_args = TermCountMapping<int>(ac_term.get_args());
                new_args.erase(one_term);

                return ONE_IDENTITY_REDUCE(bank, MLTS, std::move(new_args));
            }
        }

        return std::nullopt;
    }


    // MLTS(a ADDS(b c)) -> ADDS(MLTS(a b) MLTS(a c))
    REWRITE_COMPILED_DEF(R_MLTS2, bank, term) {

        // match MLTS(...)
        if (term->get_head() == MLTS) {
            const ACTerm<int>& ac_term = static_cast<const ACTerm<int>&>(*term);
            auto args = ac_term.get_args();

            for (const auto& [arg, count] : args) {
                // match ADDS(b c)
                if (arg->get_head() == ADDS) {
                    const ACTerm<int>& inner_ac_term = static_cast<const ACTerm<int>&>(*arg);

                    // get the arguments
                    auto rest_args = TermCountMapping<int>(args);
                    subtract_TermCountMapping(rest_args, arg, 1);

                    // start creating
                    auto new_args = TermCountMapping<int>();

                    for (const auto& [inner_arg, inner_count] : inner_ac_term.get_args()) {
                        auto new_inner_args = TermCountMapping<int>(rest_args);
                        add_TermCountMapping(new_inner_args, inner_arg, inner_count);
                        new_args[bank.get_ac_term(MLTS, std::move(new_inner_args))] = 1;
                    }

                    return bank.get_ac_term(ADDS, std::move(new_args));
                }
            }
        }

        return std::nullopt;
    }

} // namespace scalar
#include "scalar.hpp"

/*

The scalar module.

Signature: 
    - Constants: 0, 1
    - Unary Symbols: CONJ
    - AC Symbols: ADDS, MLTS

*/

namespace scalar {

    using namespace ualg;

    StringSymbolType symbols = {
        {"0", SymbolType::NORMAL},
        {"1", SymbolType::NORMAL},
        {"CONJ", SymbolType::NORMAL},
        {"ADDS", SymbolType::AC},
        {"MLTS", SymbolType::AC}
    };

    Signature<int> reserved_sig = compile_string_sig(symbols);

    int ZERO = reserved_sig.head_mapping["0"];
    int ONE = reserved_sig.head_mapping["1"];
    int CONJ = reserved_sig.head_mapping["CONJ"];
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

    // CONJ(0) -> 0
    REWRITE_COMPILED_DEF(R_CONJ0, bank, term) {
        auto zero_term = bank.get_normal_term(ZERO, {});
        auto CONJ_0_term = bank.get_normal_term(CONJ, {zero_term});

        if (term == CONJ_0_term) {
            return zero_term;
        }

        return std::nullopt;
    }

    // CONJ(1) -> 1
    REWRITE_COMPILED_DEF(R_CONJ1, bank, term) {
        auto one_term = bank.get_normal_term(ONE, {});
        auto CONJ_1_term = bank.get_normal_term(CONJ, {one_term});

        if (term == CONJ_1_term) {
            return one_term;
        }

        return std::nullopt;
    }

    // CONJ(ADDS(a b)) -> ADDS(CONJ(a) CONJ(b))
    REWRITE_COMPILED_DEF(R_CONJ2, bank, term) {
        if (term->get_head() == CONJ) {
            const NormalTerm<int>& term_CONJ_ADDS_a_b = static_cast<const NormalTerm<int>&>(*term);
            auto args_CONJ_ADDS_a_b = term_CONJ_ADDS_a_b.get_args();
            if (args_CONJ_ADDS_a_b[0]->get_head() == ADDS) {
                const ACTerm<int>& term_ADDS_a_b = static_cast<const ACTerm<int>&>(*args_CONJ_ADDS_a_b[0]);
                auto args_ADDS_a_b = term_ADDS_a_b.get_args();

                auto args_CONJ_a_CONJ_b = TermCountMapping<int>();
                for (const auto& [arg, count] : args_ADDS_a_b) {
                    args_CONJ_a_CONJ_b[bank.get_normal_term(CONJ, {arg})] = count;
                }
                return bank.get_ac_term(ADDS, std::move(args_CONJ_a_CONJ_b));
            }
        }

        return std::nullopt;
    }

    // CONJ(MLTS(a b)) -> MLTS(CONJ(a) CONJ(b))
    REWRITE_COMPILED_DEF(R_CONJ3, bank, term) {
        if (term->get_head() == CONJ) {
            const NormalTerm<int>& term_CONJ_MLTS_a_b = static_cast<const NormalTerm<int>&>(*term);
            auto args_CONJ_MLTS_a_b = term_CONJ_MLTS_a_b.get_args();
            if (args_CONJ_MLTS_a_b[0]->get_head() == MLTS) {
                const ACTerm<int>& term_MLTS_a_b = static_cast<const ACTerm<int>&>(*args_CONJ_MLTS_a_b[0]);
                auto args_MLTS_a_b = term_MLTS_a_b.get_args();

                auto args_CONJ_a_CONJ_b = TermCountMapping<int>();
                for (const auto& [arg, count] : args_MLTS_a_b) {
                    args_CONJ_a_CONJ_b[bank.get_normal_term(CONJ, {arg})] = count;
                }
                return bank.get_ac_term(MLTS, std::move(args_CONJ_a_CONJ_b));
            }
        }

        return std::nullopt;
    }

    // CONJ(CONJ(a)) -> a
    REWRITE_COMPILED_DEF(R_CONJ4, bank, term) {
        if (term->get_head() == CONJ) {
            const NormalTerm<int>& term_CONJ_CONJ_a = static_cast<const NormalTerm<int>&>(*term);
            auto args_CONJ_CONJ_a = term_CONJ_CONJ_a.get_args();
            if (args_CONJ_CONJ_a[0]->get_head() == CONJ) {
                const NormalTerm<int>& term_CONJ_a = static_cast<const NormalTerm<int>&>(*args_CONJ_CONJ_a[0]);
                auto args_CONJ_a = term_CONJ_a.get_args();
                return args_CONJ_a[0];
            }
        }

        return std::nullopt;
    }

    //////////////////////////////////////////
    // define the rule list
    const std::vector<RewritingRule<int>> scalar_rules = {
        R_ADDS0,
        R_MLTS0,
        R_MLTS1,
        R_MLTS2,
        R_CONJ0,
        R_CONJ1,
        R_CONJ2,
        R_CONJ3,
        R_CONJ4
    };

} // namespace scalar
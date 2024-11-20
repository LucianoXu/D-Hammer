#include "scalar_vec.hpp"

/*

The scalar module.

Signature: 
    - Constants: 0, 1
    - Unary Symbols: CONJ
    - AC Symbols: ADDS, MLTS

*/

namespace scalar_vec {

    using namespace ualg;

    StringSymbolType symbols = {
        {"0", SymbolType::NORMAL},
        {"1", SymbolType::NORMAL},
        {"CONJ", SymbolType::NORMAL},
        {"ADDS", SymbolType::NORMAL},
        {"MLTS", SymbolType::NORMAL}
    };

    Signature<int> reserved_sig = compile_string_sig(symbols);

    int ZERO = reserved_sig.head_mapping["0"];
    int ONE = reserved_sig.head_mapping["1"];
    int CONJ = reserved_sig.head_mapping["CONJ"];
    int ADDS = reserved_sig.head_mapping["ADDS"];
    int MLTS = reserved_sig.head_mapping["MLTS"];

    std::set<int> ac_symbols = {ADDS, MLTS};
    

    //////////////// Flattening AC symbols
    REWRITE_COMPILED_DEF(R_FLATTEN, bank, term) {
        auto res = flatten<int>(term, bank, ac_symbols);
        if (res != term) {
            return res;
        }
        return std::nullopt;
    }


    /////////////////////////////////////////////////////////////////////////
    // Properties
    REWRITE_COMPILED_DEF(R_ADDSID, bank, term) {
        ListArgs<int> args;
        if (match_normal_head(term, ADDS, args)) {
            if (args.size() == 1) {
                return args[0];
            }
        }
        return std::nullopt;
    }

    REWRITE_COMPILED_DEF(R_MLTSID, bank, term) {
        ListArgs<int> args;
        if (match_normal_head(term, MLTS, args)) {
            if (args.size() == 1) {
                return args[0];
            }
        }
        return std::nullopt;
    }

    /////////////////////////////////////////////////////////////////////////
    // Rewriting Rules

    // ADDS(a 0) -> a
    REWRITE_COMPILED_DEF(R_ADDS0, bank, term) {
        auto zero_term = bank.get_normal_term(ZERO, {});

        ListArgs<int> args_ADDS_a_0;
        if (match_normal_head(term, ADDS, args_ADDS_a_0)) {
            ListArgs<int> new_args;
            for (const auto& arg : args_ADDS_a_0) {
                if (arg == zero_term) {
                    continue;
                }
                new_args.push_back(arg);
            }
            if (new_args.size() < args_ADDS_a_0.size()) {
                return bank.get_normal_term(ADDS, std::move(new_args));
            }
        }

        return std::nullopt;
    }

    // MLTS(a 0) -> 0
    REWRITE_COMPILED_DEF(R_MLTS0, bank, term) {
        auto zero_term = bank.get_normal_term(ZERO, {});

        ListArgs<int> args_MLTS_a_0;
        if (match_normal_head(term, MLTS, args_MLTS_a_0)) {
            for (const auto& arg : args_MLTS_a_0) {
                if (arg == zero_term) {
                    return zero_term;
                }
            }
        }

        return std::nullopt;
    }

    // MLTS(a 1) -> a
    REWRITE_COMPILED_DEF(R_MLTS1, bank, term) {
        auto one_term = bank.get_normal_term(ONE, {});

        ListArgs<int> args_MLTS_a_1;
        if (match_normal_head(term, MLTS, args_MLTS_a_1)) {
            ListArgs<int> new_args;
            for (const auto& arg : args_MLTS_a_1) {
                if (arg == one_term) {
                    continue;
                }
                new_args.push_back(arg);
            }
            if (new_args.size() < args_MLTS_a_1.size()) {
                return bank.get_normal_term(MLTS, std::move(new_args));
            }
        }

        return std::nullopt;
    }


    // MLTS(a ADDS(b c)) -> ADDS(MLTS(a b) MLTS(a c))
    REWRITE_COMPILED_DEF(R_MLTS2, bank, term) {

        ListArgs<int> args_MLTS_a_ADDS_b_c;
        if (match_normal_head(term, MLTS, args_MLTS_a_ADDS_b_c)) {

            for (auto i = 0; i != args_MLTS_a_ADDS_b_c.size(); ++i) {
                ListArgs<int> args_ADDS_b_c;
                if (match_normal_head(args_MLTS_a_ADDS_b_c[i], ADDS, args_ADDS_b_c)) {
                    
                    ListArgs<int> newargs_ADDS_MLTS;
                    for (const auto& adds_arg : args_ADDS_b_c) {
                        ListArgs<int> newargs_MLTS{args_MLTS_a_ADDS_b_c};
                        newargs_MLTS[i] = adds_arg;
                        newargs_ADDS_MLTS.push_back(bank.get_normal_term(MLTS, std::move(newargs_MLTS)));
                    }

                    return bank.get_normal_term(ADDS, std::move(newargs_ADDS_MLTS));
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

        ListArgs<int> args_CONJ_ADDS_a_b;
        if (match_normal_head(term, CONJ, args_CONJ_ADDS_a_b)) {

            ListArgs<int> args_ADDS_a_b;
            if (match_normal_head(args_CONJ_ADDS_a_b[0], ADDS, args_ADDS_a_b)) {
                ListArgs<int> newargs_ADDS_CONJ;
                for (const auto& arg : args_ADDS_a_b) {
                    newargs_ADDS_CONJ.push_back(bank.get_normal_term(CONJ, {arg}));
                }
                return bank.get_normal_term(ADDS, std::move(newargs_ADDS_CONJ));
            }
        }

        return std::nullopt;
    }

    // CONJ(MLTS(a b)) -> MLTS(CONJ(a) CONJ(b))
    REWRITE_COMPILED_DEF(R_CONJ3, bank, term) {
        ListArgs<int> args_CONJ_MLTS_a_b;

        if (match_normal_head(term, CONJ, args_CONJ_MLTS_a_b)) {
            ListArgs<int> args_MLTS_a_b;
            if (match_normal_head(args_CONJ_MLTS_a_b[0], MLTS, args_MLTS_a_b)) {
                ListArgs<int> newargs_MLTS_CONJ;
                for (const auto& arg : args_MLTS_a_b) {
                    newargs_MLTS_CONJ.push_back(bank.get_normal_term(CONJ, {arg}));
                }
                return bank.get_normal_term(MLTS, std::move(newargs_MLTS_CONJ));
            }
        }

        return std::nullopt;
    }

    // CONJ(CONJ(a)) -> a
    REWRITE_COMPILED_DEF(R_CONJ4, bank, term) {
        ListArgs<int> args_CONJ_CONJ_a;
        if (match_normal_head(term, CONJ, args_CONJ_CONJ_a)) {
            ListArgs<int> args_CONJ_a;
            if (match_normal_head(args_CONJ_CONJ_a[0], CONJ, args_CONJ_a)) {
                return args_CONJ_a[0];
            }
        }

        return std::nullopt;
    }

    //////////////////////////////////////////
    // define the rule list
    const std::vector<RewritingRule<int>> scalar_rules = {
        R_FLATTEN,
        R_ADDSID,
        R_MLTSID,
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
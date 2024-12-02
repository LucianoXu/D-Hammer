#include "diracoq.hpp"

namespace diracoq {

    using namespace ualg;
    using namespace std;


    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, const NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules, NormalTermPos& current_pos) {
        
        // Check whether the rule can be applied to this term
        for (const auto& rule : rules) {
            auto apply_res = rule(kernel, term);
            if (apply_res.has_value()) {
                // return the discovered replacement
                return PosReplaceRecord{rule, current_pos, static_cast<const NormalTerm<int>*>(apply_res.value())};
            }
        }
        
        // Check whether the rule can be applied to the subterms

        for (unsigned int i = 0; i < term->get_args().size(); i++) {
            current_pos.push_back(i);
            auto subterm = static_cast<const NormalTerm<int>*>(term->get_args()[i]);
            auto replace_res = get_pos_replace(kernel, subterm, rules, current_pos);
            if (replace_res.has_value()) {
                return replace_res;
            }
            current_pos.pop_back();
        }

        return std::nullopt;        
    }

    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, const NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules) {
        NormalTermPos current_pos;
        return get_pos_replace(kernel, term, rules, current_pos);
    }

    const NormalTerm<int>* pos_rewrite_repeated(Kernel& kernel, const NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules, std::vector<PosReplaceRecord>* trace) {
        auto current_term = term;
        while (true) {
            auto replace_res = get_pos_replace(kernel, current_term, rules);
            if (replace_res.has_value()) {
                if (trace != nullptr) {
                    trace->push_back(replace_res.value());
                }
                current_term = kernel.get_bank().replace_term(current_term, replace_res.value().pos, replace_res.value().replacement);
            }
            else {
                break;
            }
        }
        return current_term;
    }

    //////////////// Rules

    DIRACOQ_RULE_DEF(R_BETA, kernel, term) {
        ListArgs<int> args;
        if (match_normal_head(term, APPLY, args)) {
            ListArgs<int> fun_args;
            if (match_normal_head(args[0], FUN, fun_args)) {
                return kernel.get_bank().replace_term(fun_args[2], {{fun_args[0], args[1]}});
            }
        }
        return std::nullopt;
    }

    DIRACOQ_RULE_DEF(R_DELTA, kernel, term) {
        auto find_res = kernel.find_in_env(term->get_head());
        if (find_res != std::nullopt and find_res->is_def()) {
            return find_res->def.value();
        }
        return std::nullopt;
    }

    DIRACOQ_RULE_DEF(R_ETA, kernel, term) {
        ListArgs<int> args;
        if (match_normal_head(term, FUN, args)) {
            ListArgs<int> fun_args;
            if (match_normal_head(args[2], APPLY, fun_args)) {
                if (fun_args[1] == args[0]) {
                    return fun_args[0];
                }
            }
        }
        return std::nullopt;
    }


    //////////////// Flattening AC symbols
    DIRACOQ_RULE_DEF(R_FLATTEN, kernel, term) {
        auto res = flatten<int>(term, kernel.get_bank(), ac_symbols);
        if (res != term) {
            return res;
        }
        return std::nullopt;
    }


    /////////////////////////////////////////////////////////////////////////
    // Properties
    DIRACOQ_RULE_DEF(R_ADDSID, kernel, term) {
        ListArgs<int> args;
        if (match_normal_head(term, ADDS, args)) {
            if (args.size() == 1) {
                return args[0];
            }
        }
        return std::nullopt;
    }

    DIRACOQ_RULE_DEF(R_MULSID, kernel, term) {
        ListArgs<int> args;
        if (match_normal_head(term, MULS, args)) {
            if (args.size() == 1) {
                return args[0];
            }
        }
        return std::nullopt;
    }

    /////////////////////////////////////////////////////////////////////////
    // Rewriting Rules

    // ADDS(a 0) -> a
    DIRACOQ_RULE_DEF(R_ADDS0, kernel, term) {
        auto& bank = kernel.get_bank();
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
            if (new_args.empty()) {
                new_args.push_back(zero_term);
            }
            if (new_args.size() < args_ADDS_a_0.size()) {
                return bank.get_normal_term(ADDS, std::move(new_args));
            }
        }

        return std::nullopt;
    }

    // MULS(a 0) -> 0
    DIRACOQ_RULE_DEF(R_MULS0, kernel, term) {
        auto& bank = kernel.get_bank();
        auto zero_term = bank.get_normal_term(ZERO, {});

        ListArgs<int> args_MULS_a_0;
        if (match_normal_head(term, MULS, args_MULS_a_0)) {
            for (const auto& arg : args_MULS_a_0) {
                if (arg == zero_term) {
                    return zero_term;
                }
            }
        }

        return std::nullopt;
    }

    // MULS(a 1) -> a
    DIRACOQ_RULE_DEF(R_MULS1, kernel, term) {
        auto& bank = kernel.get_bank();

        auto one_term = bank.get_normal_term(ONE, {});

        ListArgs<int> args_MULS_a_1;
        if (match_normal_head(term, MULS, args_MULS_a_1)) {
            ListArgs<int> new_args;
            for (const auto& arg : args_MULS_a_1) {
                if (arg == one_term) {
                    continue;
                }
                new_args.push_back(arg);
            }
            if (new_args.empty()) {
                new_args.push_back(one_term);
            }
            if (new_args.size() < args_MULS_a_1.size()) {
                return bank.get_normal_term(MULS, std::move(new_args));
            }
        }

        return std::nullopt;
    }


    // MULS(a ADDS(b c)) -> ADDS(MULS(a b) MULS(a c))
    DIRACOQ_RULE_DEF(R_MULS2, kernel, term) {
        auto& bank = kernel.get_bank();

        ListArgs<int> args_MULS_a_ADDS_b_c;
        if (match_normal_head(term, MULS, args_MULS_a_ADDS_b_c)) {

            // Does not match MULS(ADDS(...))
            if (args_MULS_a_ADDS_b_c.size() == 1) {
                return std::nullopt;
            }

            for (auto i = 0; i != args_MULS_a_ADDS_b_c.size(); ++i) {
                ListArgs<int> args_ADDS_b_c;
                if (match_normal_head(args_MULS_a_ADDS_b_c[i], ADDS, args_ADDS_b_c)) {
                    
                    ListArgs<int> newargs_ADDS_MULS;
                    for (const auto& adds_arg : args_ADDS_b_c) {
                        ListArgs<int> newargs_MULS{args_MULS_a_ADDS_b_c};
                        newargs_MULS[i] = adds_arg;
                        newargs_ADDS_MULS.push_back(bank.get_normal_term(MULS, std::move(newargs_MULS)));
                    }

                    return bank.get_normal_term(ADDS, std::move(newargs_ADDS_MULS));
                }
            }
        }

        return std::nullopt;
    }

    // CONJ(0) -> 0
    DIRACOQ_RULE_DEF(R_CONJ0, kernel, term) {
        auto& bank = kernel.get_bank();

        auto zero_term = bank.get_normal_term(ZERO, {});
        auto CONJ_0_term = bank.get_normal_term(CONJ, {zero_term});

        if (term == CONJ_0_term) {
            return zero_term;
        }

        return std::nullopt;
    }

    // CONJ(1) -> 1
    DIRACOQ_RULE_DEF(R_CONJ1, kernel, term) {
        auto& bank = kernel.get_bank();

        auto one_term = bank.get_normal_term(ONE, {});
        auto CONJ_1_term = bank.get_normal_term(CONJ, {one_term});

        if (term == CONJ_1_term) {
            return one_term;
        }

        return std::nullopt;
    }

    // CONJ(ADDS(a b)) -> ADDS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ2, kernel, term) {
        auto& bank = kernel.get_bank();

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

    // CONJ(MULS(a b)) -> MULS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ3, kernel, term) {
        auto& bank = kernel.get_bank();

        ListArgs<int> args_CONJ_MULS_a_b;

        if (match_normal_head(term, CONJ, args_CONJ_MULS_a_b)) {
            ListArgs<int> args_MULS_a_b;
            if (match_normal_head(args_CONJ_MULS_a_b[0], MULS, args_MULS_a_b)) {
                ListArgs<int> newargs_MULS_CONJ;
                for (const auto& arg : args_MULS_a_b) {
                    newargs_MULS_CONJ.push_back(bank.get_normal_term(CONJ, {arg}));
                }
                return bank.get_normal_term(MULS, std::move(newargs_MULS_CONJ));
            }
        }

        return std::nullopt;
    }

    // CONJ(CONJ(a)) -> a
    DIRACOQ_RULE_DEF(R_CONJ4, kernel, term) {

        ListArgs<int> args_CONJ_CONJ_a;
        if (match_normal_head(term, CONJ, args_CONJ_CONJ_a)) {
            ListArgs<int> args_CONJ_a;
            if (match_normal_head(args_CONJ_CONJ_a[0], CONJ, args_CONJ_a)) {
                return args_CONJ_a[0];
            }
        }

        return std::nullopt;
    }

    const std::vector<PosRewritingRule> rules = {
        R_BETA,
        R_DELTA,
        R_ETA,
        R_FLATTEN,
        R_ADDSID,
        R_MULSID,
        R_ADDS0,
        R_MULS0,
        R_MULS1,
        R_MULS2,
        R_CONJ0,
        R_CONJ1,
        R_CONJ2,
        R_CONJ3,
        R_CONJ4
    };    

} // namespace diracoq
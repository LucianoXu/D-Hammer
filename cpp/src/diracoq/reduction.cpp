#include "diracoq.hpp"

namespace diracoq {

    using namespace ualg;
    using namespace std;


    /**
     * @brief The main function to process the recursive matching. Note that the context will be changed when entering bound variable scopes.
     * 
     * @param kernel 
     * @param term 
     * @param rules 
     * @param current_pos 
     * @return std::optional<PosReplaceRecord> 
     */
    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, const Term<int>* term, const std::vector<PosRewritingRule>& rules, TermPos& current_pos) {
        auto head = term->get_head();
        auto args = term->get_args();

        // Check whether the rule can be applied to this term
        for (const auto& rule : rules) {
            auto apply_res = rule(kernel, term);
            if (apply_res.has_value()) {
                // return the discovered replacement
                return PosReplaceRecord{term, rule, current_pos, apply_res.value()};
            }
        }
        
        // Check whether the rule can be applied to the subterms
        if (head == FUN) {
            kernel.context_push(args[0]->get_head(), args[1]);
            
            current_pos.push_back(2);
            auto replace_res = get_pos_replace(kernel, args[2], rules, current_pos);

            current_pos.pop_back();

            kernel.context_pop();

            if (replace_res.has_value()) {
                return replace_res;
            }
            return std::nullopt;
        }
        else if (head == IDX) {
            kernel.context_push(args[0]->get_head(), kernel.get_bank().get_term(INDEX));
            
            current_pos.push_back(1);
            auto replace_res = get_pos_replace(kernel, args[1], rules, current_pos);
            current_pos.pop_back();

            kernel.context_pop();

            if (replace_res.has_value()) {
                return replace_res;
            }
            return std::nullopt;
        }
        else {
            for (unsigned int i = 0; i < args.size(); i++) {
                current_pos.push_back(i);
                auto replace_res = get_pos_replace(kernel, args[i], rules, current_pos);
                if (replace_res.has_value()) {
                    return replace_res;
                }
                current_pos.pop_back();
            }

            return std::nullopt;        
        }
    }

    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, const Term<int>* term, const std::vector<PosRewritingRule>& rules) {
        TermPos current_pos;
        auto res = get_pos_replace(kernel, term, rules, current_pos);

        // record the whole term in the trace
        if (res.has_value()) {
            res->init_term = term;
            return res;
        }
        else{
            return std::nullopt;
        }
    }

    const Term<int>* pos_rewrite_repeated(Kernel& kernel, const Term<int>* term, const std::vector<PosRewritingRule>& rules, std::vector<PosReplaceRecord>* trace) {
        auto current_term = term;
        while (true) {
            auto replace_res = get_pos_replace(kernel, current_term, rules);
            if (replace_res.has_value()) {
                if (trace != nullptr) {
                    trace->push_back(replace_res.value());
                }
                current_term = kernel.get_bank().replace_term_at(current_term, replace_res.value().pos, replace_res.value().replacement);
            }
            else {
                break;
            }
        }
        return current_term;
    }


    const Term<int>* variable_expand(Kernel& kernel, const Term<int>* term) {
        auto& bank = kernel.get_bank();
        auto& sig = kernel.get_sig();

        if (term->is_atomic()) {
            auto type = kernel.calc_type(term);
            auto type_head = type->get_head();
            auto type_args = type->get_args();

            // K : KTYPE(A) -> SUM(USET(A) FUN(i BASIS(A) SCR(DOT(BRA(i) K) KET(i))))
            if (type_head == KTYPE) {
                auto new_bound = bank.get_term(sig.register_symbol(unique_var()));
                return bank.get_term(
                    SUM,
                    {
                        bank.get_term(USET, {type_args[0]}),
                        bank.get_term(FUN, {
                            new_bound,
                            bank.get_term(BASIS, {type_args[0]}),
                            bank.get_term(SCR, {
                                bank.get_term(DOT, {
                                    bank.get_term(BRA, {new_bound}),
                                    term
                                }),
                                bank.get_term(KET, {new_bound})
                            })
                        })
                    }
                );
            }
            
            // B : BTYPE(A) -> SUM(USET(A) FUN(i BASIS(A) SCR(DOT(B KET(i)) BRA(i))))
            else if (type_head == BTYPE) {
                auto new_bound = bank.get_term(sig.register_symbol(unique_var()));
                return bank.get_term(
                    SUM,
                    {
                        bank.get_term(USET, {type_args[0]}),
                        bank.get_term(FUN, {
                            new_bound,
                            bank.get_term(BASIS, {type_args[0]}),
                            bank.get_term(SCR, {
                                bank.get_term(DOT, {
                                    term,
                                    bank.get_term(KET, {new_bound})
                                }),
                                bank.get_term(BRA, {new_bound})
                            })
                        })
                    }
                );
            }

            // O : BTYPE(A, B) -> SUM(USET(A) FUN(i BASIS(A) 
            //                          SUM(USET(B) FUN(j BASIS(B)
            //                              SCR(
            //                                  DOT(BRA(i) MULK(O KET(j)))
            //                                  OUTER(KET(i) BRA(j))
            //                              )
            //                          ))
            //                    ))
            else if (type_head == OTYPE) {
                auto new_bound_A = bank.get_term(sig.register_symbol(unique_var()));
                auto new_bound_B = bank.get_term(sig.register_symbol(unique_var()));
                return bank.get_term(
                    SUM,
                    {
                        bank.get_term(USET, {type_args[0]}),
                        bank.get_term(FUN, {
                            new_bound_A,
                            bank.get_term(BASIS, {type_args[0]}),
                            bank.get_term(
                                SUM,
                                {
                                    bank.get_term(USET, {type_args[1]}),
                                    bank.get_term(FUN, {
                                        new_bound_B,
                                        bank.get_term(BASIS, {type_args[1]}),
                                        bank.get_term(SCR, {
                                            bank.get_term(DOT, {
                                                bank.get_term(BRA, {new_bound_A}),
                                                bank.get_term(MULK, {term, bank.get_term(KET, {new_bound_B})})
                                            }),
                                            bank.get_term(OUTER, {bank.get_term(KET, {new_bound_A}), bank.get_term(BRA, {new_bound_B})})
                                        })
                                    })
                                }
                            )
                        })
                    }
                );
            }

            return term;
        }

        // recursive case
        else {
            auto head = term->get_head();
            auto& args = term->get_args();
            auto new_args = ListArgs<int>();

            if (head == FUN) {
                kernel.context_push(args[0]->get_head(), args[1]);
                
                new_args.push_back(args[0]);
                new_args.push_back(args[1]);
                new_args.push_back(variable_expand(kernel, args[2]));

                kernel.context_pop();
            }
            else if (head == IDX) {
                kernel.context_push(args[0]->get_head(), kernel.get_bank().get_term(INDEX));
                
                new_args.push_back(args[0]);
                new_args.push_back(variable_expand(kernel, args[1]));

                kernel.context_pop();
            }
            else {
                for (const auto& arg : args) {
                    new_args.push_back(variable_expand(kernel, arg));
                }
            }
            return bank.get_term(head, std::move(new_args));
        }

    }

    //////////////// Rules

/**
 * @brief The helper macro for the logic of matching the head of a term.
 * 
 */
#define MATCH_HEAD(term, head, subterm) \
    if (term->get_head()!=head) return std::nullopt;\
    auto& subterm = term->get_args();



    // a : STYPE, b : STYPE => a @ b -> MULS(a b)
    DIRACOQ_RULE_DEF(R_COMPO_SS, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeA = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeA->get_head() == STYPE and typeB->get_head() == STYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(MULS, args);
    }

    // a : STYPE, K : KTYPE(T) => a @ K -> SCR(a K)
    DIRACOQ_RULE_DEF(R_COMPO_SK, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeA = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeA->get_head() == STYPE and typeB->get_head() == KTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(SCR, args);
    }

    // a : STYPE, B : BTYPE(T) => a @ B -> SCR(a B)
    DIRACOQ_RULE_DEF(R_COMPO_SB, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeA = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeA->get_head() == STYPE and typeB->get_head() == BTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(SCR, args);
    }

    // a : STYPE, O : OTYPE(T1 T2) => a @ O -> SCR(a O)
    DIRACOQ_RULE_DEF(R_COMPO_SO, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeA = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeA->get_head() == STYPE and typeB->get_head() == OTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(SCR, args);
    }


    // K : KTYPE(T), a : STYPE => COMPO(K a) -> SCR(a K)
    DIRACOQ_RULE_DEF(R_COMPO_KS, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeK = kernel.calc_type(args[0]);
        auto typeA = kernel.calc_type(args[1]);

        if (!(typeK->get_head() == KTYPE and typeA->get_head() == STYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(SCR, {args[1], args[0]});
    }

    // K1 : KTYPE(T1), K2 : KTYPE(T2) => COMPO(K1 K2) -> TSR(K1 K2)
    DIRACOQ_RULE_DEF(R_COMPO_KK, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeK1 = kernel.calc_type(args[0]);
        auto typeK2 = kernel.calc_type(args[1]);

        if (!(typeK1->get_head() == KTYPE and typeK2->get_head() == KTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(TSR, args);
    }

    // K : KTYPE(T1), B : BTYPE(T2) => COMPO(K B) -> OUTER(K B)
    DIRACOQ_RULE_DEF(R_COMPO_KB, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeK = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeK->get_head() == KTYPE and typeB->get_head() == BTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(OUTER, args);
    }


    // B : BTYPE(T), a : STYPE => COMPO(B a) -> SCR(a B)
    DIRACOQ_RULE_DEF(R_COMPO_BS, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeB = kernel.calc_type(args[0]);
        auto typeA = kernel.calc_type(args[1]);

        if (!(typeB->get_head() == BTYPE and typeA->get_head() == STYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(SCR, {args[1], args[0]});
    }

    // B : BTYPE(T), K : KTYPE(T) => COMPO(B K) -> DOT(B K)
    DIRACOQ_RULE_DEF(R_COMPO_BK, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeB = kernel.calc_type(args[0]);
        auto typeK = kernel.calc_type(args[1]);

        if (!(typeB->get_head() == BTYPE and typeK->get_head() == KTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(DOT, args);
    }

    // B1 : BTYPE(T1), B2 : BTYPE(T2) => COMPO(B1 B2) -> TSR(B1 B2)
    DIRACOQ_RULE_DEF(R_COMPO_BB, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeB1 = kernel.calc_type(args[0]);
        auto typeB2 = kernel.calc_type(args[1]);

        if (!(typeB1->get_head() == BTYPE and typeB2->get_head() == BTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(TSR, args);
    }

    // B : BTYPE(T1), O : OTYPE(T1 T2) => COMPO(B O) -> MULB(B O)
    DIRACOQ_RULE_DEF(R_COMPO_BO, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeB = kernel.calc_type(args[0]);
        auto typeO = kernel.calc_type(args[1]);

        if (!(typeB->get_head() == BTYPE and typeO->get_head() == OTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(MULB, args);
    }

    // O : OTYPE(T1 T2), a : STYPE => COMPO(O a) -> SCR(a O)
    DIRACOQ_RULE_DEF(R_COMPO_OS, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeO = kernel.calc_type(args[0]);
        auto typeA = kernel.calc_type(args[1]);

        if (!(typeO->get_head() == OTYPE and typeA->get_head() == STYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(SCR, {args[1], args[0]});
    }

    // O : OTYPE(T1 T2), K : KTYPE(T2) => COMPO(O K) -> MULK(O K)
    DIRACOQ_RULE_DEF(R_COMPO_OK, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeO = kernel.calc_type(args[0]);
        auto typeK = kernel.calc_type(args[1]);

        if (!(typeO->get_head() == OTYPE and typeK->get_head() == KTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(MULK, args);
    }

    // O1 : OTYPE(T1 T2), O2 : OTYPE(T2 T3) => COMPO(O1 O2) -> MULO(O1 O2)
    DIRACOQ_RULE_DEF(R_COMPO_OO, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeO1 = kernel.calc_type(args[0]);
        auto typeO2 = kernel.calc_type(args[1]);

        if (!(typeO1->get_head() == OTYPE and typeO2->get_head() == OTYPE)) return std::nullopt;
        
        return kernel.get_bank().get_term(MULO, args);
    }

    // f : T1 -> T2 => COMPO(f a) -> APPLY(f a)
    DIRACOQ_RULE_DEF(R_COMPO_ARROW, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeF = kernel.calc_type(args[0]);

        if (!(typeF->get_head() == ARROW)) return std::nullopt;
        
        return kernel.get_bank().get_term(APPLY, args);
    }

    // f : FORALL(x T1) => COMPO(f a) -> APPLY(f a)
    DIRACOQ_RULE_DEF(R_COMPO_FORALL, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeF = kernel.calc_type(args[0]);

        if (!(typeF->get_head() == FORALL)) return std::nullopt;
        
        return kernel.get_bank().get_term(APPLY, args);
    }

    // T1 : INDEX, T2 : INDEX => STAR(T1 T2) -> PROD(T1 T2)
    DIRACOQ_RULE_DEF(R_STAR_PROD, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto typeA = kernel.calc_type(args[0]);

        if (!(typeA->get_head() == INDEX)) return std::nullopt;

        return kernel.get_bank().get_term(PROD, args);
    }

    // an : SType => STAR(a1 ... an) -> MULS(a1 ... an)
    DIRACOQ_RULE_DEF(R_STAR_MULS, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto type1 = kernel.calc_type(args[0]);

        if (!(type1->get_head() == STYPE)) return std::nullopt;

        return kernel.get_bank().get_term(MULS, args);
    }

    // O1 : OType(T1 T2), O2 : OType(T3 T3) => STAR(O1 O2) -> TSR(O1 O2)
    DIRACOQ_RULE_DEF(R_STAR_TSRO, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto typeA = kernel.calc_type(args[0]);
        
        if (!(typeA->get_head() == OTYPE)) return std::nullopt;

        return kernel.get_bank().get_term(TSR, args);
    }

    // S1 : SET(T1), S2 : SET(T2) => STAR(S1 S2) -> CATPROD(S1 S2)
    DIRACOQ_RULE_DEF(R_STAR_CATPROD, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto typeA = kernel.calc_type(args[0]);
        
        if (!(typeA->get_head() == SET)) return std::nullopt;

        return kernel.get_bank().get_term(CATPROD, args);
    }

    // an : STYPE => ADDG(a1 ... an) -> ADDS(a1 ... an)
    DIRACOQ_RULE_DEF(R_ADDG_ADDS, kernel, term) {
        MATCH_HEAD(term, ADDG, args)
        auto type1 = kernel.calc_type(args[0]);

        if (!(type1->get_head() == STYPE)) return std::nullopt;

        return kernel.get_bank().get_term(ADDS, args);
    }

    // an : KTYPE(T), BTYPE(T) or OTYPE(T1 T2) => ADDG(a1 ... an) -> ADD(a1 ... an)
    DIRACOQ_RULE_DEF(R_ADDG_ADD, kernel, term) {
        MATCH_HEAD(term, ADDG, args)
        auto type1 = kernel.calc_type(args[0]);

        if (!(type1->get_head() == KTYPE or type1->get_head() == BTYPE or type1->get_head() == OTYPE)) return std::nullopt;

        return kernel.get_bank().get_term(ADD, args);
    }

    // S : SET(T) => SSUM(i S e) -> SUM(S FUN(i BASIS(T) e))
    DIRACOQ_RULE_DEF(R_SSUM, kernel, term) {
        MATCH_HEAD(term, SSUM, args)
        auto typeS = kernel.calc_type(args[1]);

        return kernel.get_bank().get_term(SUM, {
            args[1],
            kernel.get_bank().get_term(FUN, {
                args[0],
                kernel.get_bank().get_term(BASIS, {typeS->get_args()[0]}),
                args[2]
            })
        });
    }

    const std::vector<PosRewritingRule> pre_proc_rules = {
        R_COMPO_SS, R_COMPO_SK, R_COMPO_SB, R_COMPO_SO,
        R_COMPO_KS, R_COMPO_KK, R_COMPO_KB,
        R_COMPO_BS, R_COMPO_BK, R_COMPO_BB, R_COMPO_BO,
        R_COMPO_OS, R_COMPO_OK,             R_COMPO_OO,
        R_COMPO_ARROW, R_COMPO_FORALL,
        R_STAR_PROD, R_STAR_MULS, R_STAR_TSRO, R_STAR_CATPROD,
        R_ADDG_ADDS, R_ADDG_ADD,
        R_SSUM
    };

    //////////////////////////////////////////////////////////////
    // Main rewriting rules

    DIRACOQ_RULE_DEF(R_BETA_ARROW, kernel, term) {
        MATCH_HEAD(term, APPLY, args)
        MATCH_HEAD(args[0], FUN, fun_args)
        if (fun_args.size() != 3) return std::nullopt;
        return subst(kernel.get_sig(), kernel.get_bank(), fun_args[2], fun_args[0]->get_head(), args[1]);
    }

    DIRACOQ_RULE_DEF(R_BETA_INDEX, kernel, term) {
        MATCH_HEAD(term, APPLY, args)
        MATCH_HEAD(args[0], IDX, idx_args)
        if (idx_args.size() != 2) return std::nullopt;
        return subst(kernel.get_sig(), kernel.get_bank(), idx_args[1], idx_args[0]->get_head(), args[1]);
    }

    DIRACOQ_RULE_DEF(R_DELTA, kernel, term) {
        auto find_res = kernel.find_in_env(term->get_head());
        if (find_res != std::nullopt and find_res->is_def()) {
            return find_res->def.value();
        }
        return std::nullopt;
    }

    // //////////////// Flattening AC symbols
    DIRACOQ_RULE_DEF(R_FLATTEN, kernel, term) {
        auto res = flatten<int>(term, kernel.get_bank(), a_symbols);
        if (res != term) {
            return res;
        }
        return std::nullopt;
    }


    /////////////////////////////////////////////////////////////////////////
    // Properties
    DIRACOQ_RULE_DEF(R_ADDSID, kernel, term) {
        MATCH_HEAD(term, ADDS, args)
        if (args.size() != 1) return std::nullopt;

        return args[0];
    }

    DIRACOQ_RULE_DEF(R_MULSID, kernel, term) {
        MATCH_HEAD(term, MULS, args)
        if (args.size() != 1) return std::nullopt;

        return args[0];
    }

    /////////////////////////////////////////////////////////////////////////
    // Rewriting Rules

    // ADDS(a 0) -> a
    DIRACOQ_RULE_DEF(R_ADDS0, kernel, term) {
        auto& bank = kernel.get_bank();
        auto zero_term = bank.get_term(ZERO);

        MATCH_HEAD(term, ADDS, args_ADDS_a_0)
        
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

        if (new_args.size() == args_ADDS_a_0.size()) return std::nullopt;

        return bank.get_term(ADDS, std::move(new_args));
    }

    // MULS(a 0) -> 0
    DIRACOQ_RULE_DEF(R_MULS0, kernel, term) {
        auto& bank = kernel.get_bank();
        auto zero_term = bank.get_term(ZERO);

        MATCH_HEAD(term, MULS, args_MULS_a_0)

        for (const auto& arg : args_MULS_a_0) {
            if (arg == zero_term) {
                return zero_term;
            }
        }

        return std::nullopt;
    }

    // MULS(a 1) -> a
    DIRACOQ_RULE_DEF(R_MULS1, kernel, term) {
        auto& bank = kernel.get_bank();

        auto one_term = bank.get_term(ONE);

        MATCH_HEAD(term, MULS, args_MULS_a_1)
        
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
        
        if (new_args.size() == args_MULS_a_1.size()) return std::nullopt;

        return bank.get_term(MULS, std::move(new_args));
    }


    // MULS(a ADDS(b c)) -> ADDS(MULS(a b) MULS(a c))
    DIRACOQ_RULE_DEF(R_MULS2, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, MULS, args_MULS_a_ADDS_b_c)

        // Does not match MULS(ADDS(...))
        if (args_MULS_a_ADDS_b_c.size() == 1) {
            return std::nullopt;
        }

        for (auto i = 0; i != args_MULS_a_ADDS_b_c.size(); ++i) {
            if (args_MULS_a_ADDS_b_c[i]->get_head() == ADDS) {
                auto& args_ADDS_b_c = args_MULS_a_ADDS_b_c[i]->get_args();

                ListArgs<int> newargs_ADDS_MULS;
                for (const auto& adds_arg : args_ADDS_b_c) {
                    ListArgs<int> newargs_MULS{args_MULS_a_ADDS_b_c};
                    newargs_MULS[i] = adds_arg;
                    newargs_ADDS_MULS.push_back(bank.get_term(MULS, std::move(newargs_MULS)));
                }

                return bank.get_term(ADDS, std::move(newargs_ADDS_MULS));
            }
        }

        return std::nullopt;
    }

    // CONJ(0) -> 0
    DIRACOQ_RULE_DEF(R_CONJ0, kernel, term) {
        auto& bank = kernel.get_bank();

        auto zero_term = bank.get_term(ZERO);
        auto CONJ_0_term = bank.get_term(CONJ, {zero_term});

        if (term == CONJ_0_term) {
            return zero_term;
        }

        return std::nullopt;
    }

    // CONJ(1) -> 1
    DIRACOQ_RULE_DEF(R_CONJ1, kernel, term) {
        auto& bank = kernel.get_bank();

        auto one_term = bank.get_term(ONE);
        auto CONJ_1_term = bank.get_term(CONJ, {one_term});

        if (term == CONJ_1_term) {
            return one_term;
        }

        return std::nullopt;
    }

    // CONJ(ADDS(a b)) -> ADDS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ2, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, CONJ, args_CONJ_ADDS_a_b)

        MATCH_HEAD(args_CONJ_ADDS_a_b[0], ADDS, args_ADDS_a_b)
        
        ListArgs<int> newargs_ADDS_CONJ;
        for (const auto& arg : args_ADDS_a_b) {
            newargs_ADDS_CONJ.push_back(bank.get_term(CONJ, {arg}));
        }
        return bank.get_term(ADDS, std::move(newargs_ADDS_CONJ));
    }

    // CONJ(MULS(a b)) -> MULS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ3, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, CONJ, args_CONJ_MULS_a_b)

        MATCH_HEAD(args_CONJ_MULS_a_b[0], MULS, args_MULS_a_b)
            
        ListArgs<int> newargs_MULS_CONJ;
        for (const auto& arg : args_MULS_a_b) {
            newargs_MULS_CONJ.push_back(bank.get_term(CONJ, {arg}));
        }
        return bank.get_term(MULS, std::move(newargs_MULS_CONJ));
    }

    // CONJ(CONJ(a)) -> a
    DIRACOQ_RULE_DEF(R_CONJ4, kernel, term) {

        MATCH_HEAD(term, CONJ, args_CONJ_CONJ_a)
        
        MATCH_HEAD(args_CONJ_CONJ_a[0], CONJ, args_CONJ_a)
        
        return args_CONJ_a[0];
    }

    // CONJ(DELTA(s t)) -> DELTA(s t)
    DIRACOQ_RULE_DEF(R_CONJ5, kernel, term) {

        MATCH_HEAD(term, CONJ, args_CONJ_DELTA_s_t)

        if (args_CONJ_DELTA_s_t[0]->get_head() != DELTA) return std::nullopt;
            
        return args_CONJ_DELTA_s_t[0];
    }

    // CONJ(DOT(B K)) -> DOT(ADJ(K) ADJ(B))
    DIRACOQ_RULE_DEF(R_CONJ6, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, CONJ, args_CONJ_DOT_B_K)
        
        MATCH_HEAD(args_CONJ_DOT_B_K[0], DOT, args_DOT_B_K)

        return bank.get_term(DOT,
            {
                bank.get_term(ADJ, {args_DOT_B_K[1]}),
                bank.get_term(ADJ, {args_DOT_B_K[0]})
            }
        );
    }

    // DOT(0B(sigma) K) -> 0
    DIRACOQ_RULE_DEF(R_DOT0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_0B_sigma_K)
        
        if (args_DOT_0B_sigma_K[0]->get_head() != ZEROB) return std::nullopt;
        
        return bank.get_term(ZERO);
    }

    // DOT(B 0K(sigma)) -> 0
    DIRACOQ_RULE_DEF(R_DOT1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_B_0K_sigma)

        if (args_DOT_B_0K_sigma[1]->get_head() != ZEROK) return std::nullopt;
        
        return bank.get_term(ZERO);
    }

    // DOT(SCR(a B) K) -> MULS(a DOT(B K))
    DIRACOQ_RULE_DEF(R_DOT2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_SCR_a_B_K)
        
        MATCH_HEAD(args_DOT_SCR_a_B_K[0], SCR, args_SCR_a_B)
            
        return bank.get_term(MULS, 
            {
                args_SCR_a_B[0], 
                bank.get_term(DOT, {args_SCR_a_B[1], args_DOT_SCR_a_B_K[1]})
            }
        );
    }

    // DOT(B SCR(a K)) -> MULS(a DOT(B K))
    DIRACOQ_RULE_DEF(R_DOT3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_B_SCR_a_K)
        
        MATCH_HEAD(args_DOT_B_SCR_a_K[1], SCR, args_SCR_a_K)
            
        return bank.get_term(MULS, 
            {
                args_SCR_a_K[0], 
                bank.get_term(DOT, {args_DOT_B_SCR_a_K[0], args_SCR_a_K[1]})
            }
        );
    }

    // DOT(ADD(B1 ... Bn) K) -> ADD(DOT(B1 K) ... DOT(Bn K))
    DIRACOQ_RULE_DEF(R_DOT4, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_ADD_B1_Bn_K)
        
        MATCH_HEAD(args_DOT_ADD_B1_Bn_K[0], ADD, args_ADD_B1_Bn)
            
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_B1_Bn) {
            new_args.push_back(bank.get_term(DOT, {arg, args_DOT_ADD_B1_Bn_K[1]}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // DOT(B ADD(K1 ... Kn)) -> ADD(DOT(B K1) ... DOT(B Kn))
    DIRACOQ_RULE_DEF(R_DOT5, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_B_ADD_K1_Kn)
        
        MATCH_HEAD(args_DOT_B_ADD_K1_Kn[1], ADD, args_ADD_K1_Kn)
                
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_K1_Kn) {
            new_args.push_back(bank.get_term(DOT, {args_DOT_B_ADD_K1_Kn[0], arg}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // DOT(BRA(s) KET(t)) -> DELTA(s t)
    DIRACOQ_RULE_DEF(R_DOT6, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_BRA_s_KET_t)
        
        MATCH_HEAD(args_DOT_BRA_s_KET_t[0], BRA, args_BRA_s)
            
        MATCH_HEAD(args_DOT_BRA_s_KET_t[1], KET, args_KET_t)
        
        return bank.get_term(DELTA, {args_BRA_s[0], args_KET_t[0]});
    }

    // DOT(TSR(B1 B2) KET(PAIR(s t))) -> MULS(DOT(B1 KET(s)) DOT(B2 KET(t)))
    DIRACOQ_RULE_DEF(R_DOT7, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_TSR_B1_B2_KET_PAIR_s_t)

        MATCH_HEAD(args_DOT_TSR_B1_B2_KET_PAIR_s_t[0], TSR, args_TSR_B1_B2)
        
        MATCH_HEAD(args_DOT_TSR_B1_B2_KET_PAIR_s_t[1], KET, args_KET_PAIR_s_t)

        MATCH_HEAD(args_KET_PAIR_s_t[0], PAIR, args_PAIR_s_t)

        return bank.get_term(MULS, 
            {
                bank.get_term(DOT, {args_TSR_B1_B2[0], bank.get_term(KET, {args_PAIR_s_t[0]})}),
                bank.get_term(DOT, {args_TSR_B1_B2[1], bank.get_term(KET, {args_PAIR_s_t[1]})})
            }
        );
    }

    // DOT(BRA(PAIR(s t)) TSR(K1 K2)) -> MULS(DOT(BRA(s) K1) DOT(BRA(t) K2))
    DIRACOQ_RULE_DEF(R_DOT8, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_BRA_PAIR_s_t_TSR_K1_K2)
        
        MATCH_HEAD(args_DOT_BRA_PAIR_s_t_TSR_K1_K2[0], BRA, args_BRA_PAIR_s_t)
            
        MATCH_HEAD(args_BRA_PAIR_s_t[0], PAIR, args_PAIR_s_t)
                
        MATCH_HEAD(args_DOT_BRA_PAIR_s_t_TSR_K1_K2[1], TSR, args_TSR_K1_K2)
                    
        return bank.get_term(MULS, 
            {
                bank.get_term(DOT, {bank.get_term(BRA, {args_PAIR_s_t[0]}), args_TSR_K1_K2[0]}),
                bank.get_term(DOT, {bank.get_term(BRA, {args_PAIR_s_t[1]}), args_TSR_K1_K2[1]})
            }
        );
    }

    // DOT(TSR(B1 B2) TSR(K1 K2)) -> MULS(DOT(B1 K1) DOT(B2 K2))
    DIRACOQ_RULE_DEF(R_DOT9, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_TSR_B1_B2_TSR_K1_K2)
        
        MATCH_HEAD(args_DOT_TSR_B1_B2_TSR_K1_K2[0], TSR, args_TSR_B1_B2)
            
        MATCH_HEAD(args_DOT_TSR_B1_B2_TSR_K1_K2[1], TSR, args_TSR_K1_K2)
                
        return bank.get_term(MULS, 
            {
                bank.get_term(DOT, {args_TSR_B1_B2[0], args_TSR_K1_K2[0]}),
                bank.get_term(DOT, {args_TSR_B1_B2[1], args_TSR_K1_K2[1]})
            }
        );
    }

    // DOT(MULB(B O) K) -> DOT(B MULK(O K))
    DIRACOQ_RULE_DEF(R_DOT10, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_MULB_B_O_K)
        
        MATCH_HEAD(args_DOT_MULB_B_O_K[0], MULB, args_MULB_B_O)
            
        return bank.get_term(DOT, 
            {
                args_MULB_B_O[0], 
                bank.get_term(MULK, {args_MULB_B_O[1], args_DOT_MULB_B_O_K[1]})
            }
        );
    }

    // DOT(BRA(PAIR(s t)) MULK(TSR(O1 O2) K)) -> DOT(TSR(MULB(BRA(s) O1) MULB(BRA(t) O2)) K)
    DIRACOQ_RULE_DEF(R_DOT11, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_BRA_PAIR_s_t_MULK_TSR_O1_O2_K)

        MATCH_HEAD(args_DOT_BRA_PAIR_s_t_MULK_TSR_O1_O2_K[0], BRA, args_BRA_PAIR_s_t)
        
        MATCH_HEAD(args_BRA_PAIR_s_t[0], PAIR, args_PAIR_s_t)
        
        MATCH_HEAD(args_DOT_BRA_PAIR_s_t_MULK_TSR_O1_O2_K[1], MULK, args_MULK_TSR_O1_O2_K)

        MATCH_HEAD(args_MULK_TSR_O1_O2_K[0], TSR, args_TSR_O1_O2)

        return bank.get_term(DOT, 
            {
                bank.get_term(TSR, 
                    {
                        bank.get_term(MULB, {bank.get_term(BRA, {args_PAIR_s_t[0]}), args_TSR_O1_O2[0]}),
                        bank.get_term(MULB, {bank.get_term(BRA, {args_PAIR_s_t[1]}), args_TSR_O1_O2[1]})
                    }
                ),
                args_MULK_TSR_O1_O2_K[1]
            }
        );                
    }

    // DOT(TSR(B1 B2) MULK(TSR(O1 O2) K)) -> DOT(TSR(MULB(B1 O1) MULB(B2 O2)) K)
    DIRACOQ_RULE_DEF(R_DOT12, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_TSR_B1_B2_MULK_TSR_O1_O2_K)
        
        MATCH_HEAD(args_DOT_TSR_B1_B2_MULK_TSR_O1_O2_K[0], TSR, args_TSR_B1_B2)

        MATCH_HEAD(args_DOT_TSR_B1_B2_MULK_TSR_O1_O2_K[1], MULK, args_MULK_TSR_O1_O2_K)

        MATCH_HEAD(args_MULK_TSR_O1_O2_K[0], TSR, args_TSR_O1_O2)

        return bank.get_term(DOT, 
            {
                bank.get_term(TSR, 
                    {
                        bank.get_term(MULB, {args_TSR_B1_B2[0], args_TSR_O1_O2[0]}),
                        bank.get_term(MULB, {args_TSR_B1_B2[1], args_TSR_O1_O2[1]})
                    }
                ),
                args_MULK_TSR_O1_O2_K[1]
            }
        );                
    }

    // DELTA(a a) -> 1
    DIRACOQ_RULE_DEF(R_DELTA0, kernel, term) {

        MATCH_HEAD(term, DELTA, args_DELTA_a_a)

        if (args_DELTA_a_a[1] != args_DELTA_a_a[0]) return std::nullopt;

        return kernel.get_bank().get_term(ONE);
    }

    // DELTA(PAIR(a b) PAIR(c d)) -> MULS(DELTA(a c) DELTA(b d))
    DIRACOQ_RULE_DEF(R_DELTA1, kernel, term) {

        MATCH_HEAD(term, DELTA, args_DELTA_PAIR_a_b_PAIR_c_d)
        
        MATCH_HEAD(args_DELTA_PAIR_a_b_PAIR_c_d[0], PAIR, args_PAIR_a_b)
        
        MATCH_HEAD(args_DELTA_PAIR_a_b_PAIR_c_d[1], PAIR, args_PAIR_c_d)

        return kernel.get_bank().get_term(MULS, 
            {
                kernel.get_bank().get_term(DELTA, {args_PAIR_a_b[0], args_PAIR_c_d[0]}),
                kernel.get_bank().get_term(DELTA, {args_PAIR_a_b[1], args_PAIR_c_d[1]})
            }
        );
    }
    
    // SCR(1 X) -> X
    DIRACOQ_RULE_DEF(R_SCR0, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_1_X)
        if (args_SCR_1_X[0]->get_head() != ONE) return std::nullopt;

        return args_SCR_1_X[1];
    }

    // SCR(a SCR(b X)) -> SCR(MULS(a b) X)
    DIRACOQ_RULE_DEF(R_SCR1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SCR, args_SCR_a_SCR_b_X)
        
        MATCH_HEAD(args_SCR_a_SCR_b_X[1], SCR, args_SCR_b_X)

        return bank.get_term(SCR, 
            {
                bank.get_term(MULS, {args_SCR_a_SCR_b_X[0], args_SCR_b_X[0]}),
                args_SCR_b_X[1]
            }
        );
    }

    // SCR(a ADD(X1 ... Xn)) -> ADD(SCR(a X1) ... SCR(a Xn))
    DIRACOQ_RULE_DEF(R_SCR2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SCR, args_SCR_a_ADD_X1_Xn)
        
        MATCH_HEAD(args_SCR_a_ADD_X1_Xn[1], ADD, args_ADD_X1_Xn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(bank.get_term(SCR, {args_SCR_a_ADD_X1_Xn[0], arg}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }
    
    // K : KTYPE(T) => SCR(0 K) -> 0K(T)
    DIRACOQ_RULE_DEF(R_SCRK0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SCR, args_SCR_0_K)
        
        // Check the typing of K
        auto type_K = kernel.calc_type(args_SCR_0_K[1]);

        MATCH_HEAD(type_K, KTYPE, args_KType_T)

        if (args_SCR_0_K[0]->get_head() != ZERO) return std::nullopt;

        return bank.get_term(ZEROK, {args_KType_T[0]});
    }

    // SCR(a 0K(T)) -> 0K(T)
    DIRACOQ_RULE_DEF(R_SCRK1, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_a_0K_T)
        
        if (args_SCR_a_0K_T[1]->get_head() != ZEROK) return std::nullopt;

        return args_SCR_a_0K_T[1];
    }

    // B : BTYPE(T) => SCR(0 B) -> 0B(T)
    DIRACOQ_RULE_DEF(R_SCRB0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SCR, args_SCR_0_B)
        
        // Check the typing of B
        auto type_B = kernel.calc_type(args_SCR_0_B[1]);

        MATCH_HEAD(type_B, BTYPE, args_BType_T)

        if (args_SCR_0_B[0]->get_head() != ZERO) return std::nullopt;

        return bank.get_term(ZEROB, {args_BType_T[0]});
    }

    // SCR(a 0B(T)) -> 0B(T)
    DIRACOQ_RULE_DEF(R_SCRB1, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_a_0B_T)
        
        if (args_SCR_a_0B_T[1]->get_head() != ZEROB) return std::nullopt;

        return args_SCR_a_0B_T[1];
    }

    // O : OTYPE(T1 T2) => SCR(0 O) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_SCRO0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SCR, args_SCR_0_O)
        
        // Check the typing of O
        auto type_O = kernel.calc_type(args_SCR_0_O[1]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        if (args_SCR_0_O[0]->get_head() != ZERO) return std::nullopt;

        return bank.get_term(ZEROO, {args_OType_T1_T2[0], args_OType_T1_T2[1]});
    }

    // SCR(a 0O(T1 T2)) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_SCRO1, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_a_0O_T1_T2)
        
        if (args_SCR_a_0O_T1_T2[1]->get_head() != ZEROO) return std::nullopt;

        return args_SCR_a_0O_T1_T2[1];
    }

    // ADD(X) -> X
    DIRACOQ_RULE_DEF(R_ADDID, kernel, term) {

        MATCH_HEAD(term, ADD, args_ADD_X)
        if (args_ADD_X.size() != 1) return std::nullopt;

        return args_ADD_X[0];
    }
    
    // ADD(Y1 ... X ... X ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(1 1) X))
    DIRACOQ_RULE_DEF(R_ADD0, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, ADD, args_ADD_Y1_X_X_Yn)
        
        if (args_ADD_Y1_X_X_Yn.size() < 2) return std::nullopt;

        for (int i = 0; i < args_ADD_Y1_X_X_Yn.size() - 1; i++) {
            for (int j = i + 1; j < args_ADD_Y1_X_X_Yn.size(); j++) {
                if (args_ADD_Y1_X_X_Yn[i] == args_ADD_Y1_X_X_Yn[j]) {
                    ListArgs<int> new_args;
                    for (int k = 0; k < i; k++) {
                        new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                    }
                    for (int k = i + 1; k < j; k++) {
                        new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                    }
                    for (int k = j + 1; k < args_ADD_Y1_X_X_Yn.size(); k++) {
                        new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                    }
                    new_args.push_back(
                        bank.get_term(SCR, 
                            {
                                bank.get_term(ADDS, {bank.get_term(ONE, {}), bank.get_term(ONE, {})}),
                                args_ADD_Y1_X_X_Yn[i]
                            }
                        )
                    );
                    return bank.get_term(ADD, std::move(new_args));
                }
            }
        }
        return std::nullopt;
    }

    // ADD(Y1 ... X ... SCR(a X) ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(1 a) X))
    DIRACOQ_RULE_DEF(R_ADD1, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, ADD, args_ADD_Y1_X_X_Yn)
        
        if (args_ADD_Y1_X_X_Yn.size() < 2) return std::nullopt;

        for (int i = 0; i < args_ADD_Y1_X_X_Yn.size() - 1; i++) {
            for (int j = i + 1; j < args_ADD_Y1_X_X_Yn.size(); j++) {

                if (args_ADD_Y1_X_X_Yn[j]->get_head() == SCR) {
                    auto& args_SCR_a_X = args_ADD_Y1_X_X_Yn[j]->get_args();
                    if (args_SCR_a_X[1] == args_ADD_Y1_X_X_Yn[i]) {
                        ListArgs<int> new_args;
                        for (int k = 0; k < i; k++) {
                            new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                        }
                        for (int k = i + 1; k < j; k++) {
                            new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                        }
                        for (int k = j + 1; k < args_ADD_Y1_X_X_Yn.size(); k++) {
                            new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                        }
                        new_args.push_back(
                            bank.get_term(SCR, 
                                {
                                    bank.get_term(ADDS, {bank.get_term(ONE, {}), args_SCR_a_X[0]}),
                                    args_ADD_Y1_X_X_Yn[i]
                                }
                            )
                        );
                        return bank.get_term(ADD, std::move(new_args));
                    }
                }
            }
        }
        return std::nullopt;
    }


    // ADD(Y1 ... SCR(a X) ... X ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(a 1) X))
    DIRACOQ_RULE_DEF(R_ADD2, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, ADD, args_ADD_Y1_X_X_Yn)
        
        if (args_ADD_Y1_X_X_Yn.size() < 2) return std::nullopt;

        for (int i = 0; i < args_ADD_Y1_X_X_Yn.size() - 1; i++) {

            if (args_ADD_Y1_X_X_Yn[i]->get_head() == SCR) {
                auto& args_SCR_a_X = args_ADD_Y1_X_X_Yn[i]->get_args();

                for (int j = i + 1; j < args_ADD_Y1_X_X_Yn.size(); j++) {
                    if (args_ADD_Y1_X_X_Yn[j] == args_SCR_a_X[1]) {
                        ListArgs<int> new_args;
                        for (int k = 0; k < i; k++) {
                            new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                        }
                        for (int k = i + 1; k < j; k++) {
                            new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                        }
                        for (int k = j + 1; k < args_ADD_Y1_X_X_Yn.size(); k++) {
                            new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                        }
                        new_args.push_back(
                            bank.get_term(SCR, 
                                {
                                    bank.get_term(ADDS, {args_SCR_a_X[0], bank.get_term(ONE, {})}),
                                    args_SCR_a_X[1]
                                }
                            )
                        );
                        return bank.get_term(ADD, std::move(new_args));
                    }
                }
            }
        }
        return std::nullopt;
    }

    // ADD(Y1 ... SCR(a X) ... SCR(b X) ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(a b) X))
    DIRACOQ_RULE_DEF(R_ADD3, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, ADD, args_ADD_Y1_X_X_Yn)
        
        if (args_ADD_Y1_X_X_Yn.size() < 2) return std::nullopt;

        for (int i = 0; i < args_ADD_Y1_X_X_Yn.size() - 1; i++) {
            
            if (args_ADD_Y1_X_X_Yn[i]->get_head() == SCR) {
                auto& args_SCR_a_X = args_ADD_Y1_X_X_Yn[i]->get_args();
            
                for (int j = i + 1; j < args_ADD_Y1_X_X_Yn.size(); j++) {
                    
                    if (args_ADD_Y1_X_X_Yn[j]->get_head() == SCR) {
                        auto& args_SCR_b_X = args_ADD_Y1_X_X_Yn[j]->get_args();

                        if (args_SCR_a_X[1] == args_SCR_b_X[1]) {

                            ListArgs<int> new_args;
                            for (int k = 0; k < i; k++) {
                                new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                            }
                            for (int k = i + 1; k < j; k++) {
                                new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                            }
                            for (int k = j + 1; k < args_ADD_Y1_X_X_Yn.size(); k++) {
                                new_args.push_back(args_ADD_Y1_X_X_Yn[k]);
                            }
                            new_args.push_back(
                                bank.get_term(SCR, 
                                    {
                                        bank.get_term(ADDS, {args_SCR_a_X[0], args_SCR_b_X[0]}),
                                        args_SCR_a_X[1]
                                    }
                                )
                            );
                            return bank.get_term(ADD, std::move(new_args));
                        }
                    }
                }
            }
        }
        return std::nullopt;
    }

    // ADD(K1 ... 0K(T) ... Kn) -> ADD(K1 ... Kn)
    DIRACOQ_RULE_DEF(R_ADDK0, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, ADD, args_ADD_K1_0K_T_Kn)
        
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_K1_0K_T_Kn) {
            if (arg->get_head() == ZEROK) {
                continue;
            }
            new_args.push_back(arg);
        }
        if (new_args.empty()) {
            new_args.push_back(args_ADD_K1_0K_T_Kn[0]);
        }

        if (new_args.size() == args_ADD_K1_0K_T_Kn.size()) return std::nullopt;

        return bank.get_term(ADD, std::move(new_args));
    }

    // ADD(B1 ... 0B(T) ... Bn) -> ADD(B1 ... Bn)
    DIRACOQ_RULE_DEF(R_ADDB0, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, ADD, args_ADD_B1_0B_T_Bn)
        
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_B1_0B_T_Bn) {
            if (arg->get_head() == ZEROB) {
                continue;
            }
            new_args.push_back(arg);
        }
        if (new_args.empty()) {
            new_args.push_back(args_ADD_B1_0B_T_Bn[0]);
        }

        if (new_args.size() == args_ADD_B1_0B_T_Bn.size()) return std::nullopt;

        return bank.get_term(ADD, std::move(new_args));
    }

    // ADD(O1 ... 0O(T1 T2) ... On) -> ADD(O1 ... On)
    DIRACOQ_RULE_DEF(R_ADDO0, kernel, term) {
        auto& bank = kernel.get_bank();

        MATCH_HEAD(term, ADD, args_ADD_O1_0O_T1_T2_On)
        
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_0O_T1_T2_On) {
            if (arg->get_head() == ZEROO) {
                continue;
            }
            new_args.push_back(arg);
        }
        if (new_args.empty()) {
            new_args.push_back(args_ADD_O1_0O_T1_T2_On[0]);
        }

        if (new_args.size() == args_ADD_O1_0O_T1_T2_On.size()) return std::nullopt;

        return bank.get_term(ADD, std::move(new_args));
    }

    // ADJ(ADJ(X)) -> X
    DIRACOQ_RULE_DEF(R_ADJ0, kernel, term) {

        MATCH_HEAD(term, ADJ, args_ADJ_ADJ_X)
        
        MATCH_HEAD(args_ADJ_ADJ_X[0], ADJ, args_ADJ_X)
        
        return args_ADJ_X[0];
    }

    // ADJ(SCR(a X)) -> SCR(CONJ(a) ADJ(X))
    DIRACOQ_RULE_DEF(R_ADJ1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_SCR_a_X)
        
        MATCH_HEAD(args_ADJ_SCR_a_X[0], SCR, args_SCR_a_X)
        
        return bank.get_term(SCR, 
            {
                bank.get_term(CONJ, {args_SCR_a_X[0]}),
                bank.get_term(ADJ, {args_SCR_a_X[1]})
            }
        );
    }

    // ADJ(ADD(X1 ... Xn)) -> ADD(ADJ(X1) ... ADJ(Xn))
    DIRACOQ_RULE_DEF(R_ADJ2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_ADD_X1_Xn)
        
        MATCH_HEAD(args_ADJ_ADD_X1_Xn[0], ADD, args_ADD_X1_Xn)
        
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(bank.get_term(ADJ, {arg}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // ADJ(TSR(X Y)) -> TSR(ADJ(X) ADJ(Y))
    DIRACOQ_RULE_DEF(R_ADJ3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_TSR_X_Y)
        
        MATCH_HEAD(args_ADJ_TSR_X_Y[0], TSR, args_TSR_X_Y)
        
        return bank.get_term(TSR, 
            {
                bank.get_term(ADJ, {args_TSR_X_Y[0]}),
                bank.get_term(ADJ, {args_TSR_X_Y[1]})
            }
        );
    }

    // ADJ(0B(T)) -> 0K(T)
    DIRACOQ_RULE_DEF(R_ADJK0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_0B_T)
        
        MATCH_HEAD(args_ADJ_0B_T[0], ZEROB, args_ZEROB_T)

        return bank.get_term(ZEROK, {args_ZEROB_T[0]});
    }

    // ADJ(BRA(t)) -> KET(t)
    DIRACOQ_RULE_DEF(R_ADJK1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_BRA_t)
        
        MATCH_HEAD(args_ADJ_BRA_t[0], BRA, args_BRA_t)

        return bank.get_term(KET, {args_BRA_t[0]});
    }

    // ADJ(MULB(B O)) -> MULK(ADJ(O) ADJ(B))
    DIRACOQ_RULE_DEF(R_ADJK2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_MULB_B_O)

        MATCH_HEAD(args_ADJ_MULB_B_O[0], MULB, args_MULB_B_O)

        return bank.get_term(MULK, 
            {
                bank.get_term(ADJ, {args_MULB_B_O[1]}),
                bank.get_term(ADJ, {args_MULB_B_O[0]})
            }
        );
    }


    // ADJ(0K(T)) -> 0B(T)
    DIRACOQ_RULE_DEF(R_ADJB0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_0K_T)
        
        MATCH_HEAD(args_ADJ_0K_T[0], ZEROK, args_ZEROK_T)

        return bank.get_term(ZEROB, {args_ZEROK_T[0]});
    }

    // ADJ(KET(t)) -> BRA(t)
    DIRACOQ_RULE_DEF(R_ADJB1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_KET_t)
        
        MATCH_HEAD(args_ADJ_KET_t[0], KET, args_KET_t)

        return bank.get_term(BRA, {args_KET_t[0]});
    }

    // ADJ(MULK(O K)) -> MULB(ADJ(K) ADJ(O))
    DIRACOQ_RULE_DEF(R_ADJB2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_MULK_O_K)

        MATCH_HEAD(args_ADJ_MULK_O_K[0], MULK, args_MULK_O_K)

        return bank.get_term(MULB, 
            {
                bank.get_term(ADJ, {args_MULK_O_K[1]}),
                bank.get_term(ADJ, {args_MULK_O_K[0]})
            }
        );
    }


    // ADJ(0O(T1 T2)) -> 0O(T2 T1)
    DIRACOQ_RULE_DEF(R_ADJO0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_0O_T1_T2)
        
        MATCH_HEAD(args_ADJ_0O_T1_T2[0], ZEROO, args_ZEROO_T1_T2)

        return bank.get_term(ZEROO, {args_ZEROO_T1_T2[1], args_ZEROO_T1_T2[0]});
    }

    // ADJ(1O(T)) -> 1O(T)
    DIRACOQ_RULE_DEF(R_ADJO1, kernel, term) {

        MATCH_HEAD(term, ADJ, args_ADJ_1O_T)
        
        if (args_ADJ_1O_T[0]->get_head() != ONEO) return std::nullopt;

        return args_ADJ_1O_T[0];
    }

    // ADJ(OUTER(K B)) -> OUTER(ADJ(B) ADJ(K))
    DIRACOQ_RULE_DEF(R_ADJO2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_OUTER_K_B)

        MATCH_HEAD(args_ADJ_OUTER_K_B[0], OUTER, args_OUTER_K_B)

        return bank.get_term(OUTER, 
            {
                bank.get_term(ADJ, {args_OUTER_K_B[1]}),
                bank.get_term(ADJ, {args_OUTER_K_B[0]})
            }
        );
    }

    // ADJ(MULO(O1 O2)) -> MULO(ADJ(O2) ADJ(O1))
    DIRACOQ_RULE_DEF(R_ADJO3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_MULO_O1_O2)

        MATCH_HEAD(args_ADJ_MULO_O1_O2[0], MULO, args_MULO_O1_O2)

        return bank.get_term(MULO, 
            {
                bank.get_term(ADJ, {args_MULO_O1_O2[1]}),
                bank.get_term(ADJ, {args_MULO_O1_O2[0]})
            }
        );
    }

    // TSR(SCR(a X1) X2) -> SCR(a TSR(X1 X2))
    DIRACOQ_RULE_DEF(R_TSR0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_SCR_a_X1_X2)

        MATCH_HEAD(args_TSR_SCR_a_X1_X2[0], SCR, args_SCR_a_X1)

        return bank.get_term(SCR, 
            {
                args_SCR_a_X1[0],
                bank.get_term(TSR, {args_SCR_a_X1[1], args_TSR_SCR_a_X1_X2[1]})
            }
        );
    }

    // TSR(X1 SCR(a X2)) -> SCR(a TSR(X1 X2))
    DIRACOQ_RULE_DEF(R_TSR1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_X1_SCR_a_X2)

        MATCH_HEAD(args_TSR_X1_SCR_a_X2[1], SCR, args_SCR_a_X2)

        return bank.get_term(SCR, 
            {
                args_SCR_a_X2[0],
                bank.get_term(TSR, {args_TSR_X1_SCR_a_X2[0], args_SCR_a_X2[1]})
            }
        );
    }

    // TSR(ADD(X1 ... Xn) Y) -> ADD(TSR(X1 Y) ... TSR(Xn Y))
    DIRACOQ_RULE_DEF(R_TSR2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_ADD_X1_Xn_Y)

        MATCH_HEAD(args_TSR_ADD_X1_Xn_Y[0], ADD, args_ADD_X1_Xn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(bank.get_term(TSR, {arg, args_TSR_ADD_X1_Xn_Y[1]}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // TSR(Y ADD(X1 ... Xn)) -> ADD(TSR(Y X1) ... TSR(Y Xn))
    DIRACOQ_RULE_DEF(R_TSR3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_Y_ADD_X1_Xn)

        MATCH_HEAD(args_TSR_Y_ADD_X1_Xn[1], ADD, args_ADD_X1_Xn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(bank.get_term(TSR, {args_TSR_Y_ADD_X1_Xn[0], arg}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // K : KTYPE(T2) => TSR(0K(T1) K) -> 0K(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRK0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_0K_T1_K)

        MATCH_HEAD(args_TSR_0K_T1_K[0], ZEROK, args_ZEROK_T1)

        // Check the typing of K
        auto type_K = kernel.calc_type(args_TSR_0K_T1_K[1]);

        MATCH_HEAD(type_K, KTYPE, args_KType_T2)

        return bank.get_term(ZEROK, {bank.get_term(PROD, {args_ZEROK_T1[0], args_KType_T2[0]})});
    }

    // K : KTYPE(T1) => TSR(K 0K(T2)) -> 0K(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRK1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_K_0K_T2)

        MATCH_HEAD(args_TSR_K_0K_T2[1], ZEROK, args_ZEROK_T2)

        // Check the typing of K
        auto type_K = kernel.calc_type(args_TSR_K_0K_T2[0]);

        MATCH_HEAD(type_K, KTYPE, args_KType_T1)

        return bank.get_term(ZEROK, {bank.get_term(PROD, {args_KType_T1[0], args_ZEROK_T2[0]})});
    }

    // TSR(KET(s) KET(t)) -> KET(PAIR(s t))
    DIRACOQ_RULE_DEF(R_TSRK2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_KET_s_KET_t)

        MATCH_HEAD(args_TSR_KET_s_KET_t[0], KET, args_KET_s)

        MATCH_HEAD(args_TSR_KET_s_KET_t[1], KET, args_KET_t)

        return bank.get_term(KET, {bank.get_term(PAIR, {args_KET_s[0], args_KET_t[0]})});
    }

    // B : BTYPE(T2) => TSR(0B(T1) B) -> 0B(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRB0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_0B_T1_B)

        MATCH_HEAD(args_TSR_0B_T1_B[0], ZEROB, args_ZEROB_T1)

        // Check the typing of B
        auto type_B = kernel.calc_type(args_TSR_0B_T1_B[1]);

        MATCH_HEAD(type_B, BTYPE, args_BType_T2)

        return bank.get_term(ZEROB, {bank.get_term(PROD, {args_ZEROB_T1[0], args_BType_T2[0]})});
    }

    // B : BTYPE(T1) => TSR(B 0B(T2)) -> 0B(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRB1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_B_0B_T2)

        MATCH_HEAD(args_TSR_B_0B_T2[1], ZEROB, args_ZEROB_T2)

        // Check the typing of B
        auto type_B = kernel.calc_type(args_TSR_B_0B_T2[0]);

        MATCH_HEAD(type_B, BTYPE, args_BType_T1)

        return bank.get_term(ZEROB, {bank.get_term(PROD, {args_BType_T1[0], args_ZEROB_T2[0]})});
    }
    
    // TSR(BRA(s) BRA(t)) -> BRA(PAIR(s t))
    DIRACOQ_RULE_DEF(R_TSRB2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_BRA_s_BRA_t)

        MATCH_HEAD(args_TSR_BRA_s_BRA_t[0], BRA, args_BRA_s)

        MATCH_HEAD(args_TSR_BRA_s_BRA_t[1], BRA, args_BRA_t)

        return bank.get_term(BRA, {bank.get_term(PAIR, {args_BRA_s[0], args_BRA_t[0]})});
    }

    // O : OTYPE(T3 T4) => TSR(0O(T1 T2) O) -> 0O(PROD(T1 T3) PROD(T2 T4))
    DIRACOQ_RULE_DEF(R_TSRO0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_0O_T1_T2_O)

        MATCH_HEAD(args_TSR_0O_T1_T2_O[0], ZEROO, args_ZEROO_T1_T2)

        // Check the typing of O
        auto type_O = kernel.calc_type(args_TSR_0O_T1_T2_O[1]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T3_T4)

        return bank.get_term(ZEROO, 
            {
                bank.get_term(PROD, {args_ZEROO_T1_T2[0], args_OType_T3_T4[0]}),
                bank.get_term(PROD, {args_ZEROO_T1_T2[1], args_OType_T3_T4[1]})
            }
        );
    }

    // O : OTYPE(T1 T2) => TSR(O 0O(T3 T4)) -> 0O(PROD(T1 T3) PROD(T2 T4))
    DIRACOQ_RULE_DEF(R_TSRO1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_O_0O_T3_T4)

        MATCH_HEAD(args_TSR_O_0O_T3_T4[1], ZEROO, args_ZEROO_T3_T4)

        // Check the typing of O
        auto type_O = kernel.calc_type(args_TSR_O_0O_T3_T4[0]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        return bank.get_term(ZEROO, 
            {
                bank.get_term(PROD, {args_OType_T1_T2[0], args_ZEROO_T3_T4[0]}),
                bank.get_term(PROD, {args_OType_T1_T2[1], args_ZEROO_T3_T4[1]})
            }
        );
    }

    // TSR(1O(T1) 1O(T2)) -> 1O(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRO2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_1O_T1_1O_T2)

        MATCH_HEAD(args_TSR_1O_T1_1O_T2[0], ONEO, args_ONEO_T1)

        MATCH_HEAD(args_TSR_1O_T1_1O_T2[1], ONEO, args_ONEO_T2)

        return bank.get_term(ONEO, {bank.get_term(PROD, {args_ONEO_T1[0], args_ONEO_T2[0]})});
    }

    // TSR(OUTER(K1 B1) OUTER(K2 B2)) -> OUTER(TSR(K1 K2) TSR(B1 B2))
    DIRACOQ_RULE_DEF(R_TSRO3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_OUTER_K1_B1_OUTER_K2_B2)

        MATCH_HEAD(args_TSR_OUTER_K1_B1_OUTER_K2_B2[0], OUTER, args_OUTER_K1_B1)

        MATCH_HEAD(args_TSR_OUTER_K1_B1_OUTER_K2_B2[1], OUTER, args_OUTER_K2_B2)

        return bank.get_term(OUTER, 
            {
                bank.get_term(TSR, {args_OUTER_K1_B1[0], args_OUTER_K2_B2[0]}),
                bank.get_term(TSR, {args_OUTER_K1_B1[1], args_OUTER_K2_B2[1]})
            }
        );
    }

    // MULK(0O(T1 T2) K) -> 0K(T1)
    DIRACOQ_RULE_DEF(R_MULK0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_0O_T1_T2_K)

        MATCH_HEAD(args_MULK_0O_T1_T2_K[0], ZEROO, args_ZEROO_T1_T2)

        return bank.get_term(ZEROK, {args_ZEROO_T1_T2[0]});
    }

    // O : OTYPE(T1 T2) => MULK(O 0K(T2)) -> 0K(T1)
    DIRACOQ_RULE_DEF(R_MULK1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_O_0K_T2)

        if (args_MULK_O_0K_T2[1]->get_head() != ZEROK) return std::nullopt;

        // Check the typing of O
        auto type_O = kernel.calc_type(args_MULK_O_0K_T2[0]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        return bank.get_term(ZEROK, {args_OType_T1_T2[0]});
    }

    // MULK(1O(T) K) -> K
    DIRACOQ_RULE_DEF(R_MULK2, kernel, term) {

        MATCH_HEAD(term, MULK, args_MULK_1O_T_K)

        if (args_MULK_1O_T_K[0]->get_head() != ONEO) return std::nullopt;

        return args_MULK_1O_T_K[1];
    }

    // MULK(SCR(a O) K) -> SCR(a MULK(O K))
    DIRACOQ_RULE_DEF(R_MULK3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_SCR_a_O_K)

        MATCH_HEAD(args_MULK_SCR_a_O_K[0], SCR, args_SCR_a_O)

        return bank.get_term(SCR, 
            {
                args_SCR_a_O[0],
                bank.get_term(MULK, {args_SCR_a_O[1], args_MULK_SCR_a_O_K[1]})
            }
        );
    }

    // MULK(O SCR(a K)) -> SCR(a MULK(O K))
    DIRACOQ_RULE_DEF(R_MULK4, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_O_SCR_a_K)

        MATCH_HEAD(args_MULK_O_SCR_a_K[1], SCR, args_SCR_a_K)

        return bank.get_term(SCR, 
            {
                args_SCR_a_K[0],
                bank.get_term(MULK, {args_MULK_O_SCR_a_K[0], args_SCR_a_K[1]})
            }
        );
    }

    // MULK(ADD(O1 ... On) K) -> ADD(MULK(O1 K) ... MULK(On K))
    DIRACOQ_RULE_DEF(R_MULK5, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_ADD_O1_On_K)

        MATCH_HEAD(args_MULK_ADD_O1_On_K[0], ADD, args_ADD_O1_On)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_On) {
            new_args.push_back(bank.get_term(MULK, {arg, args_MULK_ADD_O1_On_K[1]}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // MULK(O ADD(K1 ... Kn)) -> ADD(MULK(O K1) ... MULK(O Kn))
    DIRACOQ_RULE_DEF(R_MULK6, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_O_ADD_K1_Kn)

        MATCH_HEAD(args_MULK_O_ADD_K1_Kn[1], ADD, args_ADD_K1_Kn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_K1_Kn) {
            new_args.push_back(bank.get_term(MULK, {args_MULK_O_ADD_K1_Kn[0], arg}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // MULK(OUTER(K1 B) K2) -> SCR(DOT(B K2) K1)
    DIRACOQ_RULE_DEF(R_MULK7, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_OUTER_K1_B_K2)

        MATCH_HEAD(args_MULK_OUTER_K1_B_K2[0], OUTER, args_OUTER_K1_B)

        return bank.get_term(SCR, 
            {
                bank.get_term(DOT, {args_OUTER_K1_B[1], args_MULK_OUTER_K1_B_K2[1]}),
                args_OUTER_K1_B[0]
            }
        );
    }

    // MULK(MULO(O1 O2) K) -> MULK(O1 MULK(O2 K))
    DIRACOQ_RULE_DEF(R_MULK8, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_MULO_O1_O2_K)

        MATCH_HEAD(args_MULK_MULO_O1_O2_K[0], MULO, args_MULO_O1_O2)

        return bank.get_term(MULK, 
            {
                args_MULO_O1_O2[0],
                bank.get_term(MULK, {args_MULO_O1_O2[1], args_MULK_MULO_O1_O2_K[1]})
            }
        );
    }

    // MULK(TSR(O1 O2) MULK(TSR(O3 O4) K)) -> MULK(TSR(MULO(O1 O3) MULO(O2 O4)) K)
    DIRACOQ_RULE_DEF(R_MULK9, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_TSR_O1_O2_MULK_TSR_O3_O4_K)

        MATCH_HEAD(args_MULK_TSR_O1_O2_MULK_TSR_O3_O4_K[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULK_TSR_O1_O2_MULK_TSR_O3_O4_K[1], MULK, args_MULK_TSR_O3_O4_K)

        MATCH_HEAD(args_MULK_TSR_O3_O4_K[0], TSR, args_TSR_O3_O4)

        return bank.get_term(MULK, 
            {
                bank.get_term(TSR, 
                    {
                        bank.get_term(MULO, {args_TSR_O1_O2[0], args_TSR_O3_O4[0]}),
                        bank.get_term(MULO, {args_TSR_O1_O2[1], args_TSR_O3_O4[1]})
                    }
                ),
                args_MULK_TSR_O3_O4_K[1]
            }
        );
    }

    // MULK(TSR(O1 O2) KET(PAIR(s t))) -> TSR(MULK(O1 KET(s)) MULK(O2 KET(t)))
    DIRACOQ_RULE_DEF(R_MULK10, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_TSR_O1_O2_KET_PAIR_s_t)

        MATCH_HEAD(args_MULK_TSR_O1_O2_KET_PAIR_s_t[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULK_TSR_O1_O2_KET_PAIR_s_t[1], KET, args_KET_PAIR_s_t)

        MATCH_HEAD(args_KET_PAIR_s_t[0], PAIR, args_PAIR_s_t)

        return bank.get_term(TSR, 
            {
                bank.get_term(MULK, {args_TSR_O1_O2[0], bank.get_term(KET, {args_PAIR_s_t[0]})}),
                bank.get_term(MULK, {args_TSR_O1_O2[1], bank.get_term(KET, {args_PAIR_s_t[1]})})
            }
        );
    }

    // MULK(TSR(O1 O2) TSR(K1 K2)) -> TSR(MULK(O1 K1) MULK(O2 K2))
    DIRACOQ_RULE_DEF(R_MULK11, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_TSR_O1_O2_TSR_K1_K2)

        MATCH_HEAD(args_MULK_TSR_O1_O2_TSR_K1_K2[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULK_TSR_O1_O2_TSR_K1_K2[1], TSR, args_TSR_K1_K2)

        return bank.get_term(TSR, 
            {
                bank.get_term(MULK, {args_TSR_O1_O2[0], args_TSR_K1_K2[0]}),
                bank.get_term(MULK, {args_TSR_O1_O2[1], args_TSR_K1_K2[1]})
            }
        );
    }

    // MULB(B 0O(T1 T2)) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_MULB0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_B_0O_T1_T2)

        MATCH_HEAD(args_MULB_B_0O_T1_T2[1], ZEROO, args_ZEROO_T1_T2)

        return bank.get_term(ZEROB, {args_ZEROO_T1_T2[1]});
    }

    // O : OTYPE(T1 T2) => MULB(0B(T1) O) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_MULB1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_0B_T1_O)

        if (args_MULB_0B_T1_O[0]->get_head() != ZEROB) return std::nullopt;

        // Check the typing of O
        auto type_O = kernel.calc_type(args_MULB_0B_T1_O[1]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        return bank.get_term(ZEROB, {args_OType_T1_T2[1]});
    }

    // MULB(B 1O(T)) -> B
    DIRACOQ_RULE_DEF(R_MULB2, kernel, term) {

        MATCH_HEAD(term, MULB, args_MULB_B_1O_T)

        if (args_MULB_B_1O_T[1]->get_head() != ONEO) return std::nullopt;

        return args_MULB_B_1O_T[0];
    }

    // MULB(SCR(a B) O) -> SCR(a MULB(B O))
    DIRACOQ_RULE_DEF(R_MULB3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_SCR_a_B_O)

        MATCH_HEAD(args_MULB_SCR_a_B_O[0], SCR, args_SCR_a_B)

        return bank.get_term(SCR, 
            {
                args_SCR_a_B[0],
                bank.get_term(MULB, {args_SCR_a_B[1], args_MULB_SCR_a_B_O[1]})
            }
        );
    }

    // MULB(B SCR(a O)) -> SCR(a MULB(B O))
    DIRACOQ_RULE_DEF(R_MULB4, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_B_SCR_a_O)

        MATCH_HEAD(args_MULB_B_SCR_a_O[1], SCR, args_SCR_a_O)

        return bank.get_term(SCR, 
            {
                args_SCR_a_O[0],
                bank.get_term(MULB, {args_MULB_B_SCR_a_O[0], args_SCR_a_O[1]})
            }
        );
    }

    // MULB(ADD(B1 ... Bn) O) -> ADD(MULB(B1 O) ... MULB(Bn O))
    DIRACOQ_RULE_DEF(R_MULB5, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_ADD_B1_Bn_O)

        MATCH_HEAD(args_MULB_ADD_B1_Bn_O[0], ADD, args_ADD_B1_Bn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_B1_Bn) {
            new_args.push_back(bank.get_term(MULB, {arg, args_MULB_ADD_B1_Bn_O[1]}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // MULB(B ADD(O1 ... On)) -> ADD(MULB(B O1) ... MULB(B On))
    DIRACOQ_RULE_DEF(R_MULB6, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_B_ADD_O1_On)

        MATCH_HEAD(args_MULB_B_ADD_O1_On[1], ADD, args_ADD_O1_On)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_On) {
            new_args.push_back(bank.get_term(MULB, {args_MULB_B_ADD_O1_On[0], arg}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // MULB(B1 OUTER(K B2)) -> SCR(DOT(B1 K) B2)
    DIRACOQ_RULE_DEF(R_MULB7, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_B1_OUTER_K_B2)

        MATCH_HEAD(args_MULB_B1_OUTER_K_B2[1], OUTER, args_OUTER_K_B2)

        return bank.get_term(SCR, 
            {
                bank.get_term(DOT, {args_MULB_B1_OUTER_K_B2[0], args_OUTER_K_B2[0]}),
                args_OUTER_K_B2[1]
            }
        );
    }

    // MULB(B MULO(O1 O2)) -> MULB(MULB(B O1) O2)
    DIRACOQ_RULE_DEF(R_MULB8, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_B_MULO_O1_O2)

        MATCH_HEAD(args_MULB_B_MULO_O1_O2[1], MULO, args_MULO_O1_O2)

        return bank.get_term(MULB, 
            {
                bank.get_term(MULB, {args_MULB_B_MULO_O1_O2[0], args_MULO_O1_O2[0]}),
                args_MULO_O1_O2[1]
            }
        );
    }

    // MULB(MULB(B TSR(O1 O2)) TSR(O3 O4)) -> MULB(B TSR(MULO(O1 O3) MULO(O2 O4)))
    DIRACOQ_RULE_DEF(R_MULB9, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_MULB_B_TSR_O1_O2_TSR_O3_O4)

        MATCH_HEAD(args_MULB_MULB_B_TSR_O1_O2_TSR_O3_O4[0], MULB, args_MULB_B_TSR_O1_O2)

        MATCH_HEAD(args_MULB_B_TSR_O1_O2[1], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULB_MULB_B_TSR_O1_O2_TSR_O3_O4[1], TSR, args_TSR_O3_O4)

        return bank.get_term(MULB, 
            {
                args_MULB_B_TSR_O1_O2[0],
                bank.get_term(TSR, 
                    {
                        bank.get_term(MULO, {args_TSR_O1_O2[0], args_TSR_O3_O4[0]}),
                        bank.get_term(MULO, {args_TSR_O1_O2[1], args_TSR_O3_O4[1]})
                    }
                )
            }
        );
    }

    // MULB(BRA(PAIR(s t)) TSR(O1 O2)) -> TSR(MULB(BRA(s) O1) MULB(BRA(t) O2))
    DIRACOQ_RULE_DEF(R_MULB10, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_BRA_PAIR_s_t_TSR_O1_O2)

        MATCH_HEAD(args_MULB_BRA_PAIR_s_t_TSR_O1_O2[0], BRA, args_BRA_PAIR_s_t)

        MATCH_HEAD(args_BRA_PAIR_s_t[0], PAIR, args_PAIR_s_t)

        MATCH_HEAD(args_MULB_BRA_PAIR_s_t_TSR_O1_O2[1], TSR, args_TSR_O1_O2)

        return bank.get_term(TSR, 
            {
                bank.get_term(MULB, {bank.get_term(BRA, {args_PAIR_s_t[0]}), args_TSR_O1_O2[0]}),
                bank.get_term(MULB, {bank.get_term(BRA, {args_PAIR_s_t[1]}), args_TSR_O1_O2[1]})
            }
        );
    }

    // MULB(TSR(B1 B2) TSR(O1 O2)) -> TSR(MULB(B1 O1) MULB(B2 O2))
    DIRACOQ_RULE_DEF(R_MULB11, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_TSR_B1_B2_TSR_O1_O2)

        MATCH_HEAD(args_MULB_TSR_B1_B2_TSR_O1_O2[0], TSR, args_TSR_B1_B2)

        MATCH_HEAD(args_MULB_TSR_B1_B2_TSR_O1_O2[1], TSR, args_TSR_O1_O2)

        return bank.get_term(TSR, 
            {
                bank.get_term(MULB, {args_TSR_B1_B2[0], args_TSR_O1_O2[0]}),
                bank.get_term(MULB, {args_TSR_B1_B2[1], args_TSR_O1_O2[1]})
            }
        );
    }

    // B : B(T2) => OUTER(0K(T1) B) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_OUTER0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, OUTER, args_OUTER_0K_T1_B)

        MATCH_HEAD(args_OUTER_0K_T1_B[0], ZEROK, args_ZEROK_T1)

        // Check the typing of B
        auto type_B = kernel.calc_type(args_OUTER_0K_T1_B[1]);

        MATCH_HEAD(type_B, BTYPE, args_BType_T2)

        return bank.get_term(ZEROO, {args_ZEROK_T1[0], args_BType_T2[0]});
    }

    // K : K(T1) => OUTER(K 0B(T2)) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_OUTER1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, OUTER, args_OUTER_K_0B_T2)

        MATCH_HEAD(args_OUTER_K_0B_T2[1], ZEROB, args_ZEROB_T2)

        // Check the typing of K
        auto type_K = kernel.calc_type(args_OUTER_K_0B_T2[0]);

        MATCH_HEAD(type_K, KTYPE, args_KType_T1)

        return bank.get_term(ZEROO, {args_KType_T1[0], args_ZEROB_T2[0]});
    }

    // OUTER(SCR(a K) B) -> SCR(a OUTER(K B))
    DIRACOQ_RULE_DEF(R_OUTER2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, OUTER, args_OUTER_SCR_a_K_B)

        MATCH_HEAD(args_OUTER_SCR_a_K_B[0], SCR, args_SCR_a_K)

        return bank.get_term(SCR, 
            {
                args_SCR_a_K[0],
                bank.get_term(OUTER, {args_SCR_a_K[1], args_OUTER_SCR_a_K_B[1]})
            }
        );
    }

    // OUTER(K SCR(a B)) -> SCR(a OUTER(K B))
    DIRACOQ_RULE_DEF(R_OUTER3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, OUTER, args_OUTER_K_SCR_a_B)

        MATCH_HEAD(args_OUTER_K_SCR_a_B[1], SCR, args_SCR_a_B)

        return bank.get_term(SCR, 
            {
                args_SCR_a_B[0],
                bank.get_term(OUTER, {args_OUTER_K_SCR_a_B[0], args_SCR_a_B[1]})
            }
        );
    }

    // OUTER(ADD(K1 ... Kn) B) -> ADD(OUTER(K1 B) ... OUTER(Kn B))
    DIRACOQ_RULE_DEF(R_OUTER4, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, OUTER, args_OUTER_ADD_K1_Kn_B)

        MATCH_HEAD(args_OUTER_ADD_K1_Kn_B[0], ADD, args_ADD_K1_Kn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_K1_Kn) {
            new_args.push_back(bank.get_term(OUTER, {arg, args_OUTER_ADD_K1_Kn_B[1]}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // OUTER(K ADD(B1 ... Bn)) -> ADD(OUTER(K B1) ... OUTER(K Bn))
    DIRACOQ_RULE_DEF(R_OUTER5, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, OUTER, args_OUTER_K_ADD_B1_Bn)

        MATCH_HEAD(args_OUTER_K_ADD_B1_Bn[1], ADD, args_ADD_B1_Bn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_B1_Bn) {
            new_args.push_back(bank.get_term(OUTER, {args_OUTER_K_ADD_B1_Bn[0], arg}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // O : OTYPE(T2 T3) => MULO(0O(T1 T2) O) -> 0O(T1 T3)
    DIRACOQ_RULE_DEF(R_MULO0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_0O_T1_T2_O)

        MATCH_HEAD(args_MULO_0O_T1_T2_O[0], ZEROO, args_ZEROO_T1_T2)

        // Check the typing of O
        auto type_O = kernel.calc_type(args_MULO_0O_T1_T2_O[1]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T2_T3)

        return bank.get_term(ZEROO, {args_ZEROO_T1_T2[0], args_OType_T2_T3[1]});
    }

    // O : OTYPE(T1 T2) => MULO(O 0O(T2 T3)) -> 0O(T1 T3)
    DIRACOQ_RULE_DEF(R_MULO1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_O_0O_T2_T3)

        MATCH_HEAD(args_MULO_O_0O_T2_T3[1], ZEROO, args_ZEROO_T2_T3)

        // Check the typing of O
        auto type_O = kernel.calc_type(args_MULO_O_0O_T2_T3[0]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        return bank.get_term(ZEROO, {args_OType_T1_T2[0], args_ZEROO_T2_T3[1]});
    }

    // MULO(1O(T) O) -> O
    DIRACOQ_RULE_DEF(R_MULO2, kernel, term) {
        MATCH_HEAD(term, MULO, args_MULO_1O_T_O)

        if (args_MULO_1O_T_O[0]->get_head() != ONEO) return std::nullopt;

        return args_MULO_1O_T_O[1];
    }

    // MULO(O 1O(T)) -> O
    DIRACOQ_RULE_DEF(R_MULO3, kernel, term) {
        MATCH_HEAD(term, MULO, args_MULO_O_1O_T)

        if (args_MULO_O_1O_T[1]->get_head() != ONEO) return std::nullopt;

        return args_MULO_O_1O_T[0];
    }

    // MULO(OUTER(K B) O) -> OUTER(K MULB(B O))
    DIRACOQ_RULE_DEF(R_MULO4, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_OUTER_K_B_O)

        MATCH_HEAD(args_MULO_OUTER_K_B_O[0], OUTER, args_OUTER_K_B)

        return bank.get_term(OUTER, 
            {
                args_OUTER_K_B[0],
                bank.get_term(MULB, {args_OUTER_K_B[1], args_MULO_OUTER_K_B_O[1]})
            }
        );
    }

    // MULO(O OUTER(K B)) -> OUTER(MULK(O K) B)
    DIRACOQ_RULE_DEF(R_MULO5, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_O_OUTER_K_B)

        MATCH_HEAD(args_MULO_O_OUTER_K_B[1], OUTER, args_OUTER_K_B)

        return bank.get_term(OUTER, 
            {
                bank.get_term(MULK, {args_MULO_O_OUTER_K_B[0], args_OUTER_K_B[0]}),
                args_OUTER_K_B[1]
            }
        );
    }

    // MULO(SCR(a O1) O2) -> SCR(a MULO(O1 O2))
    DIRACOQ_RULE_DEF(R_MULO6, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_SCR_a_O1_O2)

        MATCH_HEAD(args_MULO_SCR_a_O1_O2[0], SCR, args_SCR_a_O1)

        return bank.get_term(SCR, 
            {
                args_SCR_a_O1[0],
                bank.get_term(MULO, {args_SCR_a_O1[1], args_MULO_SCR_a_O1_O2[1]})
            }
        );
    }

    // MULO(O1 SCR(a O2)) -> SCR(a MULO(O1 O2))
    DIRACOQ_RULE_DEF(R_MULO7, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_O1_SCR_a_O2)

        MATCH_HEAD(args_MULO_O1_SCR_a_O2[1], SCR, args_SCR_a_O2)

        return bank.get_term(SCR, 
            {
                args_SCR_a_O2[0],
                bank.get_term(MULO, {args_MULO_O1_SCR_a_O2[0], args_SCR_a_O2[1]})
            }
        );
    }

    // MULO(ADD(O1 ... On) O) -> ADD(MULO(O1 O) ... MULO(On O))
    DIRACOQ_RULE_DEF(R_MULO8, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_ADD_O1_On_O)

        MATCH_HEAD(args_MULO_ADD_O1_On_O[0], ADD, args_ADD_O1_On)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_On) {
            new_args.push_back(bank.get_term(MULO, {arg, args_MULO_ADD_O1_On_O[1]}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // MULO(O ADD(O1 ... On)) -> ADD(MULO(O O1) ... MULO(O On))
    DIRACOQ_RULE_DEF(R_MULO9, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_O_ADD_O1_On)

        MATCH_HEAD(args_MULO_O_ADD_O1_On[1], ADD, args_ADD_O1_On)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_On) {
            new_args.push_back(bank.get_term(MULO, {args_MULO_O_ADD_O1_On[0], arg}));
        }
        return bank.get_term(ADD, std::move(new_args));
    }

    // MULO(MULO(O1 O2) O3) -> MULO(O1 MULO(O2 O3))
    DIRACOQ_RULE_DEF(R_MULO10, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_MULO_O1_O2_O3)

        MATCH_HEAD(args_MULO_MULO_O1_O2_O3[0], MULO, args_MULO_O1_O2)

        return bank.get_term(MULO, 
            {
                args_MULO_O1_O2[0],
                bank.get_term(MULO, {args_MULO_O1_O2[1], args_MULO_MULO_O1_O2_O3[1]})
            }
        );
    }

    // MULO(TSR(O1 O2) TSR(O3 O4)) -> TSR(MULO(O1 O3) MULO(O2 O4))
    DIRACOQ_RULE_DEF(R_MULO11, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_TSR_O1_O2_TSR_O3_O4)

        MATCH_HEAD(args_MULO_TSR_O1_O2_TSR_O3_O4[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULO_TSR_O1_O2_TSR_O3_O4[1], TSR, args_TSR_O3_O4)

        return bank.get_term(TSR, 
            {
                bank.get_term(MULO, {args_TSR_O1_O2[0], args_TSR_O3_O4[0]}),
                bank.get_term(MULO, {args_TSR_O1_O2[1], args_TSR_O3_O4[1]})
            }
        );
    }

    // MULO(TSR(O1 O2) MULO(TSR(O3 O4) O)) -> MULO(TSR(MULO(O1 O3) MULO(O2 O4)) O)
    DIRACOQ_RULE_DEF(R_MULO12, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_TSR_O1_O2_MULO_TSR_O3_O4_O)

        MATCH_HEAD(args_MULO_TSR_O1_O2_MULO_TSR_O3_O4_O[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULO_TSR_O1_O2_MULO_TSR_O3_O4_O[1], MULO, args_MULO_TSR_O3_O4_O)

        MATCH_HEAD(args_MULO_TSR_O3_O4_O[0], TSR, args_TSR_O3_O4)

        return bank.get_term(MULO, 
            {
                bank.get_term(TSR, 
                    {
                        bank.get_term(MULO, {args_TSR_O1_O2[0], args_TSR_O3_O4[0]}),
                        bank.get_term(MULO, {args_TSR_O1_O2[1], args_TSR_O3_O4[1]})
                    }
                ),
                args_MULO_TSR_O3_O4_O[1]
            }
        );
    }

    // CATPROD(USET(T1) USET(T2)) -> USET(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_SET0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, CATPROD, args_CATPROD_USET_T1_USET_T2)

        MATCH_HEAD(args_CATPROD_USET_T1_USET_T2[0], USET, args_USET_T1)

        MATCH_HEAD(args_CATPROD_USET_T1_USET_T2[1], USET, args_USET_T2)

        return bank.get_term(USET, {bank.get_term(PROD, {args_USET_T1[0], args_USET_T2[0]})});
    }

    // SUM(s FUN(x T 0)) -> 0
    DIRACOQ_RULE_DEF(R_SUM_CONST0, kernel, term) {
        MATCH_HEAD(term, SUM, args_SUM_s_fun_x_T_0)

        MATCH_HEAD(args_SUM_s_fun_x_T_0[1], FUN, args_FUN_x_T_0)

        if (args_FUN_x_T_0[2]->get_head() != ZERO) return std::nullopt;

        return args_FUN_x_T_0[2];
    }

    // SUM(s FUN(x T1 0K(T2))) -> 0K(T2)
    DIRACOQ_RULE_DEF(R_SUM_CONST1, kernel, term) {
        MATCH_HEAD(term, SUM, args_SUM_s_fun_x_T1_0K_T2)

        MATCH_HEAD(args_SUM_s_fun_x_T1_0K_T2[1], FUN, args_FUN_x_T1_0K_T2)

        if (args_FUN_x_T1_0K_T2[2]->get_head() != ZEROK) return std::nullopt;

        return args_FUN_x_T1_0K_T2[2];
    }

    // SUM(s FUN(x T1 0B(T2))) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_SUM_CONST2, kernel, term) {
        MATCH_HEAD(term, SUM, args_SUM_s_fun_x_T1_0B_T2)

        MATCH_HEAD(args_SUM_s_fun_x_T1_0B_T2[1], FUN, args_FUN_x_T1_0B_T2)

        if (args_FUN_x_T1_0B_T2[2]->get_head() != ZEROB) return std::nullopt;

        return args_FUN_x_T1_0B_T2[2];
    }

    // SUM(s FUN(x T1 0O(T2 T3))) -> 0O(T2 T3)
    DIRACOQ_RULE_DEF(R_SUM_CONST3, kernel, term) {
        MATCH_HEAD(term, SUM, args_SUM_s_fun_x_T1_0O_T2_T3)

        MATCH_HEAD(args_SUM_s_fun_x_T1_0O_T2_T3[1], FUN, args_FUN_x_T1_0O_T2_T3)

        if (args_FUN_x_T1_0O_T2_T3[2]->get_head() != ZEROO) return std::nullopt;

        return args_FUN_x_T1_0O_T2_T3[2];
    }

    // 1O(T) -> SUM(USET(T) FUN(i T OUTER(KET(i) BRA(i))))
    DIRACOQ_RULE_DEF(R_SUM_CONST4, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ONEO, args_ONEO_T)

        auto new_var_int = kernel.register_symbol(unique_var());
        auto new_var = bank.get_term(new_var_int);

        return bank.get_term(SUM, 
            {
                bank.get_term(USET, {args_ONEO_T[0]}),
                bank.get_term(FUN, 
                    {
                        new_var,
                        bank.get_term(BASIS, {args_ONEO_T[0]}),
                        bank.get_term(OUTER, 
                            {
                                bank.get_term(KET, {new_var}),
                                bank.get_term(BRA, {new_var})
                            }
                        )
                    }
                )
            }
        );
    }

    // i free in t => SUM(USET(T) FUN(i T SUM(... DELTA(i t) ...))) -> SUM(... 1 ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SUM, args_SUM_USET_T_fun_i_T_SUM_DELTA_i_t)

        if (args_SUM_USET_T_fun_i_T_SUM_DELTA_i_t[0]->get_head() != USET) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_T_fun_i_T_SUM_DELTA_i_t[1], FUN, args_FUN_i_T_SUM_DELTA_i_t)

        const Term<int>* inner_term = args_FUN_i_T_SUM_DELTA_i_t[2];

        while (true) {
            if (inner_term->get_head() == DELTA) break;
            // continually match the function and sum inside
            
            if (inner_term->get_head() == SUM) {
                auto& args = inner_term->get_args();
            
                MATCH_HEAD(args[1], FUN, ars_fun_inside)
                inner_term = ars_fun_inside[2];
            }
            else {
                return std::nullopt;
            }
        }

        MATCH_HEAD(inner_term, DELTA, args_DELTA_i_t)

        const Term<int> *i, *t;
        // Check delta_{i, t} and whether i is free in t
        if (args_DELTA_i_t[0] == args_FUN_i_T_SUM_DELTA_i_t[0]) {
            i = args_DELTA_i_t[0];
            t = args_DELTA_i_t[1];
        }
        else if (args_DELTA_i_t[1] == args_FUN_i_T_SUM_DELTA_i_t[0]) {
            i = args_DELTA_i_t[1];
            t = args_DELTA_i_t[0];
        }
        else {
            return std::nullopt;
        }
            
        if (!free_in(t, i->get_head())) return std::nullopt;

        return bank.replace_term(args_FUN_i_T_SUM_DELTA_i_t[2], {{inner_term, bank.get_term(ONE)}});  
    }

    // i free in t => SUM(USET(T) FUN(i T SUM(... MULS(a1 ... DELTA(i t) ... an) ...))) -> SUM(... MULS(a1{i/t} ... an{i/t}) ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM1, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_T_fun_i_T_SUM_a1_DELTA_i_t_an)

        if (args_SUM_USET_T_fun_i_T_SUM_a1_DELTA_i_t_an[0]->get_head() != USET) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_T_fun_i_T_SUM_a1_DELTA_i_t_an[1], FUN, args_FUN_i_T_SUM_a1_DELTA_i_t_an)

        const Term<int>* inner_term = args_FUN_i_T_SUM_a1_DELTA_i_t_an[2];

        while (true) {
            if (inner_term->get_head() == MULS) break;
            // continually match the function and sum inside
            
            if (inner_term->get_head() == SUM) {
                auto& args = inner_term->get_args();
            
                MATCH_HEAD(args[1], FUN, ars_fun_inside)
                inner_term = ars_fun_inside[2];
            }
            else {
                return std::nullopt;
            }
        }

        MATCH_HEAD(inner_term, MULS, args_MULS_a1_DELTA_i_t_an)

        if (args_MULS_a1_DELTA_i_t_an.size() == 1) return std::nullopt;


        for (int idx_i = 0; idx_i < args_MULS_a1_DELTA_i_t_an.size(); idx_i++) {

            if (args_MULS_a1_DELTA_i_t_an[idx_i]->get_head() == DELTA) {
                auto& args_DELTA_i_t = args_MULS_a1_DELTA_i_t_an[idx_i]->get_args();

                const Term<int> *i, *t;
                // Check delta_{i, t} and whether i is free in t
                if (args_DELTA_i_t[0] == args_FUN_i_T_SUM_a1_DELTA_i_t_an[0]) {
                    i = args_DELTA_i_t[0];
                    t = args_DELTA_i_t[1];
                }
                else if (args_DELTA_i_t[1] == args_FUN_i_T_SUM_a1_DELTA_i_t_an[0]) {
                    i = args_DELTA_i_t[1];
                    t = args_DELTA_i_t[0];
                }
                else {
                    continue;
                }
                    
                
                if (!free_in(t, i->get_head())) continue;

                ListArgs<int> new_mul_args;
                for (int j = 0; j < args_MULS_a1_DELTA_i_t_an.size(); j++) {
                    if (j == idx_i) {
                        continue;
                    }
                    else {
                        new_mul_args.push_back(subst(sig, bank, args_MULS_a1_DELTA_i_t_an[j], i->get_head(), t));
                    }
                }


                return bank.replace_term(args_FUN_i_T_SUM_a1_DELTA_i_t_an[2], {{inner_term, bank.get_term(MULS, std::move(new_mul_args))}});  

            }
        }

        return std::nullopt;
    }

    // i free in t => SUM(USET(T) FUN(i T SUM(... SCR(DELTA(i t) A) ...))) -> SUM(... A{i/t} ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM2, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_T_fun_i_T_SUM_SCR_DELTA_i_t_A)

        if (args_SUM_USET_T_fun_i_T_SUM_SCR_DELTA_i_t_A[0]->get_head() != USET) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_T_fun_i_T_SUM_SCR_DELTA_i_t_A[1], FUN, args_FUN_i_T_SUM_SCR_DELTA_i_t_A)

        const Term<int>* inner_term = args_FUN_i_T_SUM_SCR_DELTA_i_t_A[2];

        while (true) {
            if (inner_term->get_head() == SCR) break;
            // continually match the function and sum inside
            
            if (inner_term->get_head() == SUM) {
                auto& args = inner_term->get_args();
                
                MATCH_HEAD(args[1], FUN, ars_fun_inside)
                inner_term = ars_fun_inside[2];
            }
            else {
                return std::nullopt;
            }
        }

        MATCH_HEAD(inner_term, SCR, args_SCR_DELTA_i_t_A)

        MATCH_HEAD(args_SCR_DELTA_i_t_A[0], DELTA, args_DELTA_i_t)

        const Term<int> *i, *t;
        // Check delta_{i, t} and whether i is free in t
        if (args_DELTA_i_t[0] == args_FUN_i_T_SUM_SCR_DELTA_i_t_A[0]) {
            i = args_DELTA_i_t[0];
            t = args_DELTA_i_t[1];
        }
        else if (args_DELTA_i_t[1] == args_FUN_i_T_SUM_SCR_DELTA_i_t_A[0]) {
            i = args_DELTA_i_t[1];
            t = args_DELTA_i_t[0];
        }
        else {
            return std::nullopt;
        }
            
        if (!free_in(t, i->get_head())) return std::nullopt;

        return bank.replace_term(
            args_FUN_i_T_SUM_SCR_DELTA_i_t_A[2], 
            {{
                inner_term, 
                subst(sig, bank, args_SCR_DELTA_i_t_A[1], i->get_head(), t)
            }}
        );
    }

    // i free in t => SUM(USET(T) FUN(i T SUM(... SCR(MULS(a1 ... DELTA(i t) ... an) A) ...))) -> SUM(... SCR(MULS(a1{i/t} ... an{i/t}) A{i/t}) ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM3, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_T_fun_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A)

        if (args_SUM_USET_T_fun_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[0]->get_head() != USET) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_T_fun_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[1], FUN, args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A)

        const Term<int>* inner_term = args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[2];

        while (true) {
            if (inner_term->get_head() == SCR) break;
            // continually match the function and sum inside
            
            if (inner_term->get_head() == SUM) {
                auto& args = inner_term->get_args();
                
                MATCH_HEAD(args[1], FUN, ars_fun_inside)
                inner_term = ars_fun_inside[2];
            }
            else {
                return std::nullopt;
            }
        }

        MATCH_HEAD(inner_term, SCR, args_SCR_MULS_a1_DELTA_i_t_an_A)

        MATCH_HEAD(args_SCR_MULS_a1_DELTA_i_t_an_A[0], MULS, args_MULS_a1_DELTA_i_t_an)

        if (args_MULS_a1_DELTA_i_t_an.size() == 1) return std::nullopt;

        for (int idx_i = 0; idx_i < args_MULS_a1_DELTA_i_t_an.size(); idx_i++) {

            if (args_MULS_a1_DELTA_i_t_an[idx_i]->get_head() == DELTA) {
                auto& args_DELTA_i_t = args_MULS_a1_DELTA_i_t_an[idx_i]->get_args();

                const Term<int> *i, *t;
                // Check delta_{i, t} and whether i is free in t
                if (args_DELTA_i_t[0] == args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[0]) {
                    i = args_DELTA_i_t[0];
                    t = args_DELTA_i_t[1];
                }
                else if (args_DELTA_i_t[1] == args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[0]) {
                    i = args_DELTA_i_t[1];
                    t = args_DELTA_i_t[0];
                }
                else {
                    continue;
                }

                if (!free_in(t, i->get_head())) continue;

                ListArgs<int> new_mul_args;
                for (int j = 0; j < args_MULS_a1_DELTA_i_t_an.size(); j++) {
                    if (j == idx_i) {
                        continue;
                    }
                    else {
                        new_mul_args.push_back(subst(sig, bank, args_MULS_a1_DELTA_i_t_an[j], i->get_head(), t));
                    }
                }

                return bank.replace_term(
                    args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[2], 
                    {{
                        inner_term, 
                        bank.get_term(SCR, 
                            {
                                bank.get_term(MULS, std::move(new_mul_args)), 
                                subst(sig, bank, args_SCR_MULS_a1_DELTA_i_t_an_A[1], i->get_head(), t)
                            }
                        )
                    }}
                );
            }
        }

        return std::nullopt;
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... DELTA(i j) ...))))) -> SUM(M FUN(j T SUM(... 1 ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM4, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j)

        MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j)

        MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[2], SUM, args_SUM_M_fun_j_T_SUM_DELTA_i_j)

        // Check that the summation set is the same

        if (args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[0] != args_SUM_M_fun_j_T_SUM_DELTA_i_j[0]) return std::nullopt;

        MATCH_HEAD(args_SUM_M_fun_j_T_SUM_DELTA_i_j[1], FUN, args_FUN_j_T_SUM_DELTA_i_j)

        const Term<int>* inner_term = args_FUN_j_T_SUM_DELTA_i_j[2];

        while (true) {
            if (inner_term->get_head() == DELTA) break;
            // continually match the function and sum inside
            
            if (inner_term->get_head() == SUM) {
                auto& args = inner_term->get_args();
                
                MATCH_HEAD(args[1], FUN, ars_fun_inside)
                inner_term = ars_fun_inside[2];
            }
            else {
                return std::nullopt;
            }
        }

        MATCH_HEAD(inner_term, DELTA, args_DELTA_i_j)

        // Check whether delta variables i and j match the bound variables
        if (!((args_DELTA_i_j[0] == args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[0] && args_DELTA_i_j[1] == args_FUN_j_T_SUM_DELTA_i_j[0]) || 
            (args_DELTA_i_j[1] == args_FUN_j_T_SUM_DELTA_i_j[0] && args_DELTA_i_j[0] == args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[0]))) return std::nullopt;

        return bank.replace_term(args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[2], {{inner_term, bank.get_term(ONE, {})}});
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... MULS(a1 ... DELTA(i j) ... an) ...))))) -> SUM(M FUN(j T SUM(... MULS(a1{j/i} ... an{j/i}) ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM5, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an)

        MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an)

        MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[2], SUM, args_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an)

        // Check that the summation set is the same

        if (args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0] != args_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0]) return std::nullopt;

        MATCH_HEAD(args_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[1], FUN, args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an)

        const Term<int>* inner_term = args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an[2];

        while (true) {
            if (inner_term->get_head() == MULS) break;
            // continually match the function and sum inside
            
            if (inner_term->get_head() == SUM) {
                auto& args = inner_term->get_args();
                
                MATCH_HEAD(args[1], FUN, ars_fun_inside)
                inner_term = ars_fun_inside[2];
            }
            else {
                return std::nullopt;
            }
        }

        MATCH_HEAD(inner_term, MULS, args_MULS_a1_DELTA_i_j_an)

        if (args_MULS_a1_DELTA_i_j_an.size() == 1) return std::nullopt;

        for (int idx_i = 0; idx_i < args_MULS_a1_DELTA_i_j_an.size(); idx_i++) {
            
            if (args_MULS_a1_DELTA_i_j_an[idx_i]->get_head() == DELTA) {
                auto& args_DELTA_i_j = args_MULS_a1_DELTA_i_j_an[idx_i]->get_args();

                // Check whether delta variables i and j match the bound variables
                if (!((args_DELTA_i_j[0] == args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0] && args_DELTA_i_j[1] == args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an[0]) || 
                    (args_DELTA_i_j[1] == args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an[0] && args_DELTA_i_j[0] == args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0]))) continue;

                ListArgs<int> new_mul_args;
                for (int j = 0; j < args_MULS_a1_DELTA_i_j_an.size(); j++) {
                    if (j == idx_i) {
                        continue;
                    }
                    else {
                        new_mul_args.push_back(
                            subst(sig, bank,
                                args_MULS_a1_DELTA_i_j_an[j],
                                args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an[0]->get_head(),
                                args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0])
                        );
                    }
                }

                return bank.replace_term(
                    args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[2], 
                    {{
                        inner_term, 
                        bank.get_term(MULS, std::move(new_mul_args))
                    }}
                );
            }
        }

        return std::nullopt;
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... SCR(DELTA(i j) A) ...))))) -> SUM(M FUN(j T SUM(... A{j/i} ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM6, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A)

        MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A)

        MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[2], SUM, args_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A)

        // Check that the summation set is the same

        if (args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0] != args_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0]) return std::nullopt;

        MATCH_HEAD(args_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[1], FUN, args_FUN_j_T_SUM_DELTA_i_j_A)

        const Term<int>* inner_term = args_FUN_j_T_SUM_DELTA_i_j_A[2];

        while (true) {
            if (inner_term->get_head() == SCR) break;
            // continually match the function and sum inside
            
            if (inner_term->get_head() == SUM) {
                auto& args = inner_term->get_args();
                
                MATCH_HEAD(args[1], FUN, ars_fun_inside)
                inner_term = ars_fun_inside[2];
            }
            else {
                return std::nullopt;
            }
        }

        MATCH_HEAD(inner_term, SCR, args_SCR_DELTA_i_j_A)

        MATCH_HEAD(args_SCR_DELTA_i_j_A[0], DELTA, args_DELTA_i_j)

        // Check whether delta variables i and j match the bound variables
        if (!((args_DELTA_i_j[0] == args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0] && args_DELTA_i_j[1] == args_FUN_j_T_SUM_DELTA_i_j_A[0]) || 
            (args_DELTA_i_j[1] == args_FUN_j_T_SUM_DELTA_i_j_A[0] && args_DELTA_i_j[0] == args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0]))) return std::nullopt;

        return bank.replace_term(
            args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[2], 
            {{
                inner_term, 
                subst(sig, bank, args_SCR_DELTA_i_j_A[1], args_FUN_j_T_SUM_DELTA_i_j_A[0]->get_head(), args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0])
            }}
        );
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... SCR(MULS(a1 ... DELTA(i j) ... an) A) ...))))) -> SUM(M FUN(j T SUM(... SCR(MULS(a1{j/i} ... an{j/i}) A{j/i}) ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM7, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A)

        MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A)

        MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[2], SUM, args_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A)

        // Check that the summation set is the same

        if (args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0] != args_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]) return std::nullopt;

        MATCH_HEAD(args_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[1], FUN, args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A)

        const Term<int>* inner_term = args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[2];

        while (true) {
            if (inner_term->get_head() == SCR) break;
            // continually match the function and sum inside
            
            if (inner_term->get_head() == SUM) {
                auto& args = inner_term->get_args();
            
                MATCH_HEAD(args[1], FUN, ars_fun_inside)
                inner_term = ars_fun_inside[2];
            }
            else {
                return std::nullopt;
            }
        }

        MATCH_HEAD(inner_term, SCR, args_SCR_MULS_a1_DELTA_i_j_an_A)

        MATCH_HEAD(args_SCR_MULS_a1_DELTA_i_j_an_A[0], MULS, args_MULS_a1_DELTA_i_j_an)

        if (args_MULS_a1_DELTA_i_j_an.size() == 1) return std::nullopt;

        for (int idx_i = 0; idx_i < args_MULS_a1_DELTA_i_j_an.size(); idx_i++) {
            
            if (args_MULS_a1_DELTA_i_j_an[idx_i]->get_head() == DELTA) {
                auto& args_DELTA_i_j = args_MULS_a1_DELTA_i_j_an[idx_i]->get_args();

                // Check whether delta variables i and j match the bound variables
                if (!((args_DELTA_i_j[0] == args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0] && args_DELTA_i_j[1] == args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]) || 
                    (args_DELTA_i_j[1] == args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0] && args_DELTA_i_j[0] == args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]))) continue;

                ListArgs<int> new_mul_args;
                for (int j = 0; j < args_MULS_a1_DELTA_i_j_an.size(); j++) {
                    if (j == idx_i) {
                        continue;
                    }
                    else {
                        new_mul_args.push_back(
                            subst(sig, bank,
                                args_MULS_a1_DELTA_i_j_an[j],
                                args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]->get_head(),
                                args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0])
                        );
                    }
                }

                return bank.replace_term(
                    args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[2], 
                    {{
                        inner_term, 
                        bank.get_term(SCR, 
                            {
                                bank.get_term(MULS, std::move(new_mul_args)), 
                                subst(
                                    sig, bank, 
                                    args_SCR_MULS_a1_DELTA_i_j_an_A[1], 
                                    args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]->get_head(), 
                                    args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]
                                )
                            }
                        )
                    }}
                );
            }
        }

        return std::nullopt;
    }

    // MULS(b1 ... SUM(M FUN(i T a)) ... bn) -> SUM(M FUN(i T MULS(b1 ... a ... bn)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULS, args_MULS_b1_SUM_M_fun_i_T_a_bn)

        if (args_MULS_b1_SUM_M_fun_i_T_a_bn.size() == 1) return std::nullopt;

        for (int idx_i = 0; idx_i < args_MULS_b1_SUM_M_fun_i_T_a_bn.size(); idx_i++) {
            
            if (args_MULS_b1_SUM_M_fun_i_T_a_bn[idx_i]->get_head() != SUM) continue;
            auto& args_SUM_M_fun_i_T_a = args_MULS_b1_SUM_M_fun_i_T_a_bn[idx_i]->get_args();
            
            if (args_SUM_M_fun_i_T_a[1]->get_head() != FUN) continue;
            auto& args_fun_i_T_a = args_SUM_M_fun_i_T_a[1]->get_args();


            ListArgs<int> new_mul_args;
            for (int j = 0; j < args_MULS_b1_SUM_M_fun_i_T_a_bn.size(); j++) {
                if (j == idx_i) {
                    new_mul_args.push_back(args_fun_i_T_a[2]);
                }
                else {
                    new_mul_args.push_back(args_MULS_b1_SUM_M_fun_i_T_a_bn[j]);
                }
            }

            return bank.get_term(SUM, 
                {
                    args_SUM_M_fun_i_T_a[0],
                    bank.get_term(FUN, 
                        {
                            args_fun_i_T_a[0],
                            args_fun_i_T_a[1],
                            bank.get_term(MULS, std::move(new_mul_args))
                        }
                    )
                }
            );
        }

        return std::nullopt;          
    }

    // CONJ(SUM(M FUN(i T a))) -> SUM(M FUN(i T CONJ(a)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH1, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, CONJ, args_CONJ_SUM_M_fun_i_T_a)

        MATCH_HEAD(args_CONJ_SUM_M_fun_i_T_a[0], SUM, args_SUM_M_fun_i_T_a)

        MATCH_HEAD(args_SUM_M_fun_i_T_a[1], FUN, args_FUN_i_T_a)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_a[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_a[0],
                        args_FUN_i_T_a[1],
                        bank.get_term(CONJ, {args_FUN_i_T_a[2]})
                    }
                )
            }
        );
    }

    // ADJ(SUM(M FUN(i T X))) -> SUM(M FUN(i T ADJ(X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH2, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, ADJ, args_ADJ_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_ADJ_SUM_M_fun_i_T_X[0], SUM, args_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SUM_M_fun_i_T_X[1], FUN, args_FUN_i_T_X)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_X[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_X[0],
                        args_FUN_i_T_X[1],
                        bank.get_term(ADJ, {args_FUN_i_T_X[2]})
                    }
                )
            }
        );
    }

    // SCR(a SUM(M FUN(i T X))) -> SUM(M FUN(i T SCR(a X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH3, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SCR, args_SCR_a_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SCR_a_SUM_M_fun_i_T_X[1], SUM, args_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SUM_M_fun_i_T_X[1], FUN, args_FUN_i_T_X)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_X[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_X[0],
                        args_FUN_i_T_X[1],
                        bank.get_term(SCR, {args_SCR_a_SUM_M_fun_i_T_X[0], args_FUN_i_T_X[2]})
                    }
                )
            }
        );
    }

    // SCR(SUM(M FUN(i T a)) X) -> SUM(M FUN(i T SCR(a X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH4, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SCR, args_SCR_SUM_M_fun_i_T_a_X)

        MATCH_HEAD(args_SCR_SUM_M_fun_i_T_a_X[0], SUM, args_SUM_M_fun_i_T_a)

        MATCH_HEAD(args_SUM_M_fun_i_T_a[1], FUN, args_FUN_i_T_a)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_a[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_a[0],
                        args_FUN_i_T_a[1],
                        bank.get_term(SCR, {args_FUN_i_T_a[2], args_SCR_SUM_M_fun_i_T_a_X[1]})
                    }
                )
            }
        );
    }


    // DOT(SUM(M FUN(i T B)) K) -> SUM(M FUN(i T DOT(B K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH5, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_SUM_M_fun_i_T_B_K)

        MATCH_HEAD(args_DOT_SUM_M_fun_i_T_B_K[0], SUM, args_SUM_M_fun_i_T_B)

        MATCH_HEAD(args_SUM_M_fun_i_T_B[1], FUN, args_FUN_i_T_B)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_B[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_B[0],
                        args_FUN_i_T_B[1],
                        bank.get_term(DOT, {args_FUN_i_T_B[2], args_DOT_SUM_M_fun_i_T_B_K[1]})
                    }
                )
            }
        );
    }

    // MULK(SUM(M FUN(i T O)) K) -> SUM(M FUN(i T MULK(O K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH6, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_SUM_M_fun_i_T_O_K)

        MATCH_HEAD(args_MULK_SUM_M_fun_i_T_O_K[0], SUM, args_SUM_M_fun_i_T_O)

        MATCH_HEAD(args_SUM_M_fun_i_T_O[1], FUN, args_FUN_i_T_O)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_O[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_O[0],
                        args_FUN_i_T_O[1],
                        bank.get_term(MULK, {args_FUN_i_T_O[2], args_MULK_SUM_M_fun_i_T_O_K[1]})
                    }
                )
            }
        );
    }

    // MULB(SUM(M FUN(i T B)) O) -> SUM(M FUN(i T MULB(B O)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH7, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_SUM_M_fun_i_T_B_O)

        MATCH_HEAD(args_MULB_SUM_M_fun_i_T_B_O[0], SUM, args_SUM_M_fun_i_T_B)

        MATCH_HEAD(args_SUM_M_fun_i_T_B[1], FUN, args_FUN_i_T_B)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_B[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_B[0],
                        args_FUN_i_T_B[1],
                        bank.get_term(MULB, {args_FUN_i_T_B[2], args_MULB_SUM_M_fun_i_T_B_O[1]})
                    }
                )
            }
        );
    }
    
    // OUTER(SUM(M FUN(i T K)) B) -> SUM(M FUN(i T OUTER(K B)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH8, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, OUTER, args_OUTER_SUM_M_fun_i_T_K_B)

        MATCH_HEAD(args_OUTER_SUM_M_fun_i_T_K_B[0], SUM, args_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_SUM_M_fun_i_T_K[1], FUN, args_FUN_i_T_K)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_K[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_K[0],
                        args_FUN_i_T_K[1],
                        bank.get_term(OUTER, {args_FUN_i_T_K[2], args_OUTER_SUM_M_fun_i_T_K_B[1]})
                    }
                )
            }
        );
    }
    
    // MULO(SUM(M FUN(i T O1)) O2) -> SUM(M FUN(i T MULO(O1 O2)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH9, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_SUM_M_fun_i_T_O1_O2)

        MATCH_HEAD(args_MULO_SUM_M_fun_i_T_O1_O2[0], SUM, args_SUM_M_fun_i_T_O1)

        MATCH_HEAD(args_SUM_M_fun_i_T_O1[1], FUN, args_FUN_i_T_O1)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_O1[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_O1[0],
                        args_FUN_i_T_O1[1],
                        bank.get_term(MULO, {args_FUN_i_T_O1[2], args_MULO_SUM_M_fun_i_T_O1_O2[1]})
                    }
                )
            }
        );
    }


    // DOT(B SUM(M FUN(i T K))) -> SUM(M FUN(i T DOT(B K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH10, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, DOT, args_DOT_B_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_DOT_B_SUM_M_fun_i_T_K[1], SUM, args_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_SUM_M_fun_i_T_K[1], FUN, args_FUN_i_T_K)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_K[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_K[0],
                        args_FUN_i_T_K[1],
                        bank.get_term(DOT, {args_DOT_B_SUM_M_fun_i_T_K[0], args_FUN_i_T_K[2]})
                    }
                )
            }
        );
    }

    // MULK(O SUM(M FUN(i T K))) -> SUM(M FUN(i T MULK(O K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH11, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULK, args_MULK_O_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_MULK_O_SUM_M_fun_i_T_K[1], SUM, args_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_SUM_M_fun_i_T_K[1], FUN, args_FUN_i_T_K)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_K[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_K[0],
                        args_FUN_i_T_K[1],
                        bank.get_term(MULK, {args_MULK_O_SUM_M_fun_i_T_K[0], args_FUN_i_T_K[2]})
                    }
                )
            }
        );
    }

    // MULB(B SUM(M FUN(i T O))) -> SUM(M FUN(i T MULB(B O)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH12, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULB, args_MULB_B_SUM_M_fun_i_T_O)

        MATCH_HEAD(args_MULB_B_SUM_M_fun_i_T_O[1], SUM, args_SUM_M_fun_i_T_O)

        MATCH_HEAD(args_SUM_M_fun_i_T_O[1], FUN, args_FUN_i_T_O)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_O[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_O[0],
                        args_FUN_i_T_O[1],
                        bank.get_term(MULB, {args_MULB_B_SUM_M_fun_i_T_O[0], args_FUN_i_T_O[2]})
                    }
                )
            }
        );
    }

    // OUTER(K SUM(M FUN(i T B))) -> SUM(M FUN(i T OUTER(K B)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH13, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, OUTER, args_OUTER_K_SUM_M_fun_i_T_B)

        MATCH_HEAD(args_OUTER_K_SUM_M_fun_i_T_B[1], SUM, args_SUM_M_fun_i_T_B)

        MATCH_HEAD(args_SUM_M_fun_i_T_B[1], FUN, args_FUN_i_T_B)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_B[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_B[0],
                        args_FUN_i_T_B[1],
                        bank.get_term(OUTER, {args_OUTER_K_SUM_M_fun_i_T_B[0], args_FUN_i_T_B[2]})
                    }
                )
            }
        );
    }

    // MULO(O1 SUM(M FUN(i T O2)) -> SUM(M FUN(i T MULO(O1 O2)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH14, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, MULO, args_MULO_O1_SUM_M_fun_i_T_O2)

        MATCH_HEAD(args_MULO_O1_SUM_M_fun_i_T_O2[1], SUM, args_SUM_M_fun_i_T_O2)

        MATCH_HEAD(args_SUM_M_fun_i_T_O2[1], FUN, args_FUN_i_T_O2)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_O2[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_O2[0],
                        args_FUN_i_T_O2[1],
                        bank.get_term(MULO, {args_MULO_O1_SUM_M_fun_i_T_O2[0], args_FUN_i_T_O2[2]})
                    }
                )
            }
        );
    }


    // TSR(SUM(M FUN(i T X)) Y) -> SUM(M FUN(i T TSR(X Y)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH15, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_SUM_M_fun_i_T_X_Y)

        MATCH_HEAD(args_TSR_SUM_M_fun_i_T_X_Y[0], SUM, args_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SUM_M_fun_i_T_X[1], FUN, args_FUN_i_T_X)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_X[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_X[0],
                        args_FUN_i_T_X[1],
                        bank.get_term(TSR, {args_FUN_i_T_X[2], args_TSR_SUM_M_fun_i_T_X_Y[1]})
                    }
                )
            }
        );
    }

    // TSR(X SUM(M FUN(i T Y))) -> SUM(M FUN(i T TSR(X Y)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH16, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, TSR, args_TSR_X_SUM_M_fun_i_T_Y)

        MATCH_HEAD(args_TSR_X_SUM_M_fun_i_T_Y[1], SUM, args_SUM_M_fun_i_T_Y)

        MATCH_HEAD(args_SUM_M_fun_i_T_Y[1], FUN, args_FUN_i_T_Y)

        return bank.get_term(SUM, 
            {
                args_SUM_M_fun_i_T_Y[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_i_T_Y[0],
                        args_FUN_i_T_Y[1],
                        bank.get_term(TSR, {args_TSR_X_SUM_M_fun_i_T_Y[0], args_FUN_i_T_Y[2]})
                    }
                )
            }
        );
    }


    // SUM(M FUN(i T ADDS(a1 ... an))) -> ADDS(SUM(M FUN(i T a1)) ... SUM(M FUN(i T an)))
    DIRACOQ_RULE_DEF(R_SUM_ADDS0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_ADDS_a1_an)

        MATCH_HEAD(args_SUM_M_fun_i_T_ADDS_a1_an[1], FUN, args_FUN_i_T_ADDS_a1_an)

        MATCH_HEAD(args_FUN_i_T_ADDS_a1_an[2], ADDS, args_ADDS_a1_an)

        ListArgs<int> new_sum_args;
        for (const auto &arg : args_ADDS_a1_an) {
            new_sum_args.push_back(bank.get_term(SUM, 
                {
                    args_SUM_M_fun_i_T_ADDS_a1_an[0],
                    bank.get_term(FUN, 
                        {
                            args_FUN_i_T_ADDS_a1_an[0],
                            args_FUN_i_T_ADDS_a1_an[1],
                            arg
                        }
                    )
                }
            ));
        }

        return bank.get_term(ADDS, std::move(new_sum_args));
    }

    // SUM(M FUN(i T ADD(a1 ... an))) -> ADD(SUM(M FUN(i T a1)) ... SUM(M FUN(i T an)))
    DIRACOQ_RULE_DEF(R_SUM_ADD0, kernel, term) {
        auto &bank = kernel.get_bank();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_ADD_a1_an)

        MATCH_HEAD(args_SUM_M_fun_i_T_ADD_a1_an[1], FUN, args_FUN_i_T_ADD_a1_an)

        MATCH_HEAD(args_FUN_i_T_ADD_a1_an[2], ADD, args_ADD_a1_an)

        ListArgs<int> new_sum_args;
        for (const auto &arg : args_ADD_a1_an) {
            new_sum_args.push_back(bank.get_term(SUM, 
                {
                    args_SUM_M_fun_i_T_ADD_a1_an[0],
                    bank.get_term(FUN, 
                        {
                            args_FUN_i_T_ADD_a1_an[0],
                            args_FUN_i_T_ADD_a1_an[1],
                            arg
                        }
                    )
                }
            ));
        }

        return bank.get_term(ADD, std::move(new_sum_args));
    }

    // SUM(USET(PROD(T1 T2)) FUN(i PROD(T1 T2) X)) -> SUM(USET(T1) FUN(j T1 SUM(USET(T2) FUN(k T2 X{i/PAIR(j k)}))))
    DIRACOQ_RULE_DEF(R_SUM_INDEX0, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_Prod_T1_T2_fun_i_Prod_T1_T2_X)

        MATCH_HEAD(args_SUM_USET_Prod_T1_T2_fun_i_Prod_T1_T2_X[0], USET, args_USET_Prod_T1_T2)

        MATCH_HEAD(args_USET_Prod_T1_T2[0], PROD, args_Prod_T1_T2)

        MATCH_HEAD(args_SUM_USET_Prod_T1_T2_fun_i_Prod_T1_T2_X[1], FUN, args_FUN_i_Prod_T1_T2_X)

        const Term<int> *j = bank.get_term(kernel.register_symbol(unique_var()));
        const Term<int> *k = bank.get_term(kernel.register_symbol(unique_var()));

        return bank.get_term(SUM, 
            {
                bank.get_term(USET, {args_Prod_T1_T2[0]}),
                bank.get_term(FUN, 
                    {
                        j,
                        args_Prod_T1_T2[0],
                        bank.get_term(SUM, 
                            {
                                bank.get_term(USET, {args_Prod_T1_T2[1]}),
                                bank.get_term(FUN, 
                                    {
                                        k,
                                        args_Prod_T1_T2[1],
                                        subst(sig, bank, args_FUN_i_Prod_T1_T2_X[2], args_FUN_i_Prod_T1_T2_X[0]->get_head(), bank.get_term(PAIR, {j, k}))
                                    }
                                )
                            }
                        )
                    }
                )
            }
        );              
    }

    // SUM(CATPROD(M1 M2) FUN(i PROD(T1 T2) X)) -> SUM(M1 FUN(j T1 SUM(M2 FUN(k T2 X{i/PAIR(j k)})))
    DIRACOQ_RULE_DEF(R_SUM_INDEX1, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_CATPROD_M1_M2_fun_i_Prod_T1_T2_X)

        MATCH_HEAD(args_SUM_CATPROD_M1_M2_fun_i_Prod_T1_T2_X[0], CATPROD, args_CATPROD_M1_M2)

        MATCH_HEAD(args_SUM_CATPROD_M1_M2_fun_i_Prod_T1_T2_X[1], FUN, args_FUN_i_Prod_T1_T2_X)

        MATCH_HEAD(args_FUN_i_Prod_T1_T2_X[1], PROD, args_Prod_T1_T2_X)

        const Term<int> *j = bank.get_term(kernel.register_symbol(unique_var()));
        const Term<int> *k = bank.get_term(kernel.register_symbol(unique_var()));

        return bank.get_term(SUM, 
            {
                args_CATPROD_M1_M2[0], 
                bank.get_term(FUN, 
                    {
                        j,
                        args_Prod_T1_T2_X[0],
                        bank.get_term(SUM, 
                            {
                                args_CATPROD_M1_M2[1],
                                bank.get_term(FUN, 
                                    {
                                        k,
                                        args_Prod_T1_T2_X[1],
                                        subst(sig, bank, args_FUN_i_Prod_T1_T2_X[2], args_FUN_i_Prod_T1_T2_X[0]->get_head(), bank.get_term(PAIR, {j, k}))
                                    }
                                )
                            }
                        )
                    }
                )
            }
        );
    }

    // M1 < M2 => SUM(M2 FUN(i T1 SUM(M1 FUN(j T2 X))) -> SUM(M1 FUN(j T2 SUM(M2 FUN(i T1 X))))
    DIRACOQ_RULE_DEF(R_SUM_SWAP, kernel, term) {
        auto &bank = kernel.get_bank();
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M2_fun_i_T1_SUM_M1_fun_j_T2_X)

        MATCH_HEAD(args_SUM_M2_fun_i_T1_SUM_M1_fun_j_T2_X[1], FUN, args_FUN_i_T1_SUM_M1_fun_j_T2_X)

        MATCH_HEAD(args_FUN_i_T1_SUM_M1_fun_j_T2_X[2], SUM, args_SUM_M1_fun_j_T2_X)

        if (!is_smaller(sig, bank, args_SUM_M1_fun_j_T2_X[0], args_SUM_M2_fun_i_T1_SUM_M1_fun_j_T2_X[0])) return std::nullopt;

        MATCH_HEAD(args_SUM_M1_fun_j_T2_X[1], FUN, args_FUN_j_T2_X)

        return bank.get_term(SUM, 
            {
                args_SUM_M1_fun_j_T2_X[0],
                bank.get_term(FUN, 
                    {
                        args_FUN_j_T2_X[0],
                        args_FUN_j_T2_X[1],
                        bank.get_term(SUM, 
                            {
                                args_SUM_M2_fun_i_T1_SUM_M1_fun_j_T2_X[0],
                                bank.get_term(FUN, 
                                    {
                                        args_FUN_i_T1_SUM_M1_fun_j_T2_X[0],
                                        args_FUN_i_T1_SUM_M1_fun_j_T2_X[1],
                                        args_FUN_j_T2_X[2]
                                    }
                                )
                            }
                        )
                    }
                )
            }
        );
    }

    const std::vector<PosRewritingRule> rules = {
        R_BETA_ARROW, R_BETA_INDEX, R_DELTA, R_FLATTEN,
        
        R_ADDSID, R_MULSID, R_ADDS0, R_MULS0, R_MULS1, R_MULS2,

        R_CONJ0, R_CONJ1, R_CONJ2, R_CONJ3, R_CONJ4, R_CONJ5, R_CONJ6,
        R_DOT0, R_DOT1, R_DOT2, R_DOT3, R_DOT4, R_DOT5, R_DOT6, R_DOT7, R_DOT8, R_DOT9, R_DOT10, R_DOT11, R_DOT12, 
        R_DELTA0, R_DELTA1,

        R_SCR0, R_SCR1, R_SCR2, R_SCRK0, R_SCRK1, R_SCRB0, R_SCRB1, R_SCRO0, R_SCRO1,

        R_ADDID, R_ADD0, R_ADD1, R_ADD2, R_ADD3, R_ADDK0, R_ADDB0, R_ADDO0,

        R_ADJ0, R_ADJ1, R_ADJ2, R_ADJ3, R_ADJK0, R_ADJK1, R_ADJK2, R_ADJB0, R_ADJB1, R_ADJB2, R_ADJO0, R_ADJO1, R_ADJO2, R_ADJO3,

        R_TSR0, R_TSR1, R_TSR2, R_TSR3, R_TSRK0, R_TSRK1, R_TSRK2, R_TSRB0, R_TSRB1, R_TSRB2, R_TSRO0, R_TSRO1, R_TSRO2, R_TSRO3,

        R_MULK0, R_MULK1, R_MULK2, R_MULK3, R_MULK4, R_MULK5, R_MULK6, R_MULK7, R_MULK8, R_MULK9, R_MULK10, R_MULK11,

        R_MULB0, R_MULB1, R_MULB2, R_MULB3, R_MULB4, R_MULB5, R_MULB6, R_MULB7, R_MULB8, R_MULB9, R_MULB10, R_MULB11,

        R_OUTER0, R_OUTER1, R_OUTER2, R_OUTER3, R_OUTER4, R_OUTER5,

        R_MULO0, R_MULO1, R_MULO2, R_MULO3, R_MULO4, R_MULO5, R_MULO6, R_MULO7, R_MULO8, R_MULO9, R_MULO10, R_MULO11, R_MULO12,

        R_SET0, R_SUM_CONST0, R_SUM_CONST1, R_SUM_CONST2, R_SUM_CONST3, R_SUM_CONST4,

        R_SUM_ELIM0, R_SUM_ELIM1, R_SUM_ELIM2, R_SUM_ELIM3, R_SUM_ELIM4, R_SUM_ELIM5, R_SUM_ELIM6, R_SUM_ELIM7,

        R_SUM_PUSH0, R_SUM_PUSH1, R_SUM_PUSH2, R_SUM_PUSH3, R_SUM_PUSH4, R_SUM_PUSH5, R_SUM_PUSH6, R_SUM_PUSH7, R_SUM_PUSH8, R_SUM_PUSH9, R_SUM_PUSH10, R_SUM_PUSH11, R_SUM_PUSH12, R_SUM_PUSH13, R_SUM_PUSH14, R_SUM_PUSH15, R_SUM_PUSH16,

        R_SUM_ADDS0, R_SUM_ADD0, R_SUM_INDEX0, R_SUM_INDEX1, R_SUM_SWAP
    };

    std::vector<PosRewritingRule> get_all_rules() {
        // return the concatenation of pre_proc_rules and rules
        std::vector<PosRewritingRule> all_rules;
        all_rules.reserve(pre_proc_rules.size() + rules.size());
        all_rules.insert(all_rules.end(), pre_proc_rules.begin(), pre_proc_rules.end());
        all_rules.insert(all_rules.end(), rules.begin(), rules.end());
        return all_rules;
    }

    const std::vector<PosRewritingRule> all_rules = get_all_rules();

} // namespace diracoq
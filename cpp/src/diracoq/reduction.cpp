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
    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, TermPtr<int> term, const std::vector<PosRewritingRule>& rules, TermPos& current_pos) {
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

            // check whether the type can be rewritten
            current_pos.push_back(1);
            auto replace_res = get_pos_replace(kernel, args[1], rules, current_pos);
            current_pos.pop_back();
            if (replace_res.has_value()) {
                return replace_res;
            }

            // check whether the body can be rewritten (with context push)
            kernel.context_push(args[0]->get_head(), args[1]);
            current_pos.push_back(2);
            replace_res = get_pos_replace(kernel, args[2], rules, current_pos);
            current_pos.pop_back();
            kernel.context_pop();

            if (replace_res.has_value()) {
                return replace_res;
            }

            return std::nullopt;
        }
        else if (head == IDX) {
            kernel.context_push(args[0]->get_head(), create_term(INDEX));
            
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

    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, TermPtr<int> term, const std::vector<PosRewritingRule>& rules) {
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

    TermPtr<int> pos_rewrite_repeated(Kernel& kernel, TermPtr<int> term, const std::vector<PosRewritingRule>& rules, std::vector<PosReplaceRecord>* trace) {
        auto current_term = term;
        while (true) {
            auto replace_res = get_pos_replace(kernel, current_term, rules);
            if (replace_res.has_value()) {

                if (trace != nullptr) {
                    trace->push_back(replace_res.value());
                }
                current_term = current_term->replace_at(replace_res->pos, replace_res->replacement);


                // DEBUG PURPOSE
                // cout << pos_replace_record_to_string(kernel, replace_res.value()) << endl;
                // cout << kernel.term_to_string(current_term) << endl;
                // cout << endl;
            }
            else {
                break;
            }
        }
        return current_term;
    }

    TermPtr<int> bound_variable_rename(Kernel& kernel, TermPtr<int> term) {
        if (term->is_atomic()) {
            return term;
        }

        auto head = term->get_head();
        auto& args = term->get_args();
        auto new_args = ListArgs<int>();

        auto &sig = kernel.get_sig();

        if (head == FUN) {
            // substitute inner bound variable first
            auto type = bound_variable_rename(kernel, args[1]);
            auto body = bound_variable_rename(kernel, args[2]);

            auto new_bound = create_term(sig.register_symbol(sig.unique_var()));

            new_args.push_back(new_bound);
            new_args.push_back(subst(sig, type, args[0]->get_head(), new_bound));
            new_args.push_back(subst(sig, body, args[0]->get_head(), new_bound));
        }
        else if (head == IDX) {
            // substitute inner bound variable first
            auto body = bound_variable_rename(kernel, args[1]);

            auto new_bound = create_term(sig.register_symbol(sig.unique_var()));

            new_args.push_back(new_bound);
            new_args.push_back(subst(sig, body, args[0]->get_head(), new_bound));
        }
        else if (head == SSUM) {
            // substitute inner bound variable first
            auto set = bound_variable_rename(kernel, args[1]);
            auto body = bound_variable_rename(kernel, args[2]);

            auto new_bound = create_term(sig.register_symbol(sig.unique_var()));

            new_args.push_back(new_bound);
            new_args.push_back(subst(sig, set, args[0]->get_head(), new_bound));
            new_args.push_back(subst(sig, body, args[0]->get_head(), new_bound));
        }
        else {
            for (const auto& arg : args) {
                new_args.push_back(bound_variable_rename(kernel, arg));
            }
        }

        return create_term(head, std::move(new_args));
    }


    TermPtr<int> variable_expand(Kernel& kernel, TermPtr<int> term) {
        auto& sig = kernel.get_sig();

        auto head = term->get_head();

        // expand atomic terms
        if (term->is_atomic() || head == APPLY){
            auto type = kernel.calc_type(term);
            auto type_head = type->get_head();
            auto type_args = type->get_args();

            // K : KTYPE(A) -> SUM(USET(A) FUN(i BASIS(A) SCR(DOT(BRA(i) K) KET(i))))
            if (type_head == KTYPE) {
                auto new_bound = create_term(sig.register_symbol(sig.unique_var()));
                return create_term(
                    SUM,
                    {
                        create_term(USET, {type_args[0]}),
                        create_term(FUN, {
                            new_bound,
                            create_term(BASIS, {type_args[0]}),
                            create_term(SCR, {
                                create_term(DOT, {
                                    create_term(BRA, {new_bound}),
                                    term
                                }),
                                create_term(KET, {new_bound})
                            })
                        })
                    }
                );
            }
            
            // B : BTYPE(A) -> SUM(USET(A) FUN(i BASIS(A) SCR(DOT(B KET(i)) BRA(i))))
            else if (type_head == BTYPE) {
                auto new_bound = create_term(sig.register_symbol(sig.unique_var()));
                return create_term(
                    SUM,
                    {
                        create_term(USET, {type_args[0]}),
                        create_term(FUN, {
                            new_bound,
                            create_term(BASIS, {type_args[0]}),
                            create_term(SCR, {
                                create_term(DOT, {
                                    term,
                                    create_term(KET, {new_bound})
                                }),
                                create_term(BRA, {new_bound})
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
                auto new_bound_A = create_term(sig.register_symbol(sig.unique_var()));
                auto new_bound_B = create_term(sig.register_symbol(sig.unique_var()));

                return create_term(
                    SUM,
                    {
                        create_term(USET, {type_args[0]}),
                        create_term(FUN, {
                            new_bound_A,
                            create_term(BASIS, {type_args[0]}),
                            create_term(
                                SUM,
                                {
                                    create_term(USET, {type_args[1]}),
                                    create_term(FUN, {
                                        new_bound_B,
                                        create_term(BASIS, {type_args[1]}),
                                        create_term(SCR, {
                                            create_term(DOT, {
                                                create_term(BRA, {new_bound_A}),
                                                create_term(MULK, {term, create_term(KET, {new_bound_B})})
                                            }),
                                            create_term(OUTER, {create_term(KET, {new_bound_A}), create_term(BRA, {new_bound_B})})
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
                kernel.context_push(args[0]->get_head(), create_term(INDEX));
                
                new_args.push_back(args[0]);
                new_args.push_back(variable_expand(kernel, args[1]));

                kernel.context_pop();
            }
            else {
                for (const auto& arg : args) {
                    new_args.push_back(variable_expand(kernel, arg));
                }
            }
            return create_term(head, std::move(new_args));
        }

    }


    void _get_bound_vars(TermPtr<int> term, std::set<int>& res) {
        if (term->is_atomic()) {
            return;
        }

        auto head = term->get_head();
        auto args = term->get_args();

        if (head == FUN) {
            res.insert(args[0]->get_head());
        }

        for (const auto& arg : args) {
            _get_bound_vars(arg, res);
        }
    }

    std::set<int> get_bound_vars(TermPtr<int> term) {
        std::set<int> res;
        _get_bound_vars(term, res);
        return res;
    }


    enum COMPARE_RESULT {
        LESS,
        EQUAL,
        GREATER
    };

    /**
     * @brief Helper function to compare two terms modulo bound variables. The bound_vars should be provided as a set.
     * 
     * @param termA 
     * @param termB 
     * @param bound_vars 
     * @return COMPARE_RESULT 
     */
    COMPARE_RESULT _comp_modulo_bound_vars(TermPtr<int> termA, TermPtr<int> termB, std::set<int> bound_vars) {
        auto headA = termA->get_head();
        auto headB = termB->get_head();

        auto argsA = termA->get_args();
        auto argsB = termB->get_args();


        // bound variables are always larger than free variables, and they are equal to each other in order

        if (bound_vars.find(headA) != bound_vars.end()) {
            // set headA to the largest int
            headA = std::numeric_limits<int>::max();
        }

        if (bound_vars.find(headB) != bound_vars.end()) {
            // set headB to the largest int
            headB = std::numeric_limits<int>::max();
        }

        // start comparing

        if (headA != headB) {
            return (headA < headB)? LESS : GREATER;
        }

        auto shortest_len = std::min(argsA.size(), argsB.size());

        for (int i = 0; i < shortest_len; i++) {
            auto res = _comp_modulo_bound_vars(argsA[i], argsB[i], bound_vars);
            if (res != EQUAL) {
                return res;
            }
        }

        // if the first shortest_len elements are equal, then the shorter one is smaller

        if (argsA.size() != argsB.size()) {
            return (argsA.size() < argsB.size())? LESS : GREATER;
        }

        return EQUAL;
    }


    bool comp_modulo_bound_vars(TermPtr<int> termA, TermPtr<int> termB, const std::set<int>& bound_vars) {

        return _comp_modulo_bound_vars(termA, termB, bound_vars) == LESS;
    }
    

    /**
     * @brief Iteratively traverse the term to get the order of the bound variables. Used in the get_order_of_bound_vars.
     * 
     * Note that if some bound variable does not appear in the term, it will not be included in this order. Therefore other procedure is needed to assign proper order to these variables (according to sets of summation).
     * 
     * @param term 
     * @param vars_set 
     * @param vars 
     */
    void _iter_for_order(TermPtr<int> term, std::set<int>& vars_set, std::vector<int>& vars, std::map<int, TermPtr<int>>& var_to_sumset) {
        // NOTE: the set is used to store the bound variables that require ordering

        if (term->is_atomic()) {
            if (vars_set.find(term->get_head()) != vars_set.end()) {
                vars.push_back(term->get_head());
                vars_set.erase(term->get_head());
            }
            return;
        }

        auto head = term->get_head();
        auto args = term->get_args();

        if (head == FUN) {
            if (vars_set.find(args[0]->get_head()) == vars_set.end()) {
                vars_set.insert(args[0]->get_head());
            }
            else {
                throw std::runtime_error("The bound variable is used twice.");
            }

            _iter_for_order(args[2], vars_set, vars, var_to_sumset);
        }
        else {
            for (const auto& arg : args) {
                _iter_for_order(arg, vars_set, vars, var_to_sumset);
            }
        }

        // preserve teh sum sets for the bound variables
        if (head == SUM) {
            auto fun = args[1];
            if (fun->get_head() == FUN) {
                auto var = fun->get_args()[0]->get_head();
                if (var_to_sumset.find(var) == var_to_sumset.end()) {
                    var_to_sumset[var] = args[0];
                }
                else {
                    throw std::runtime_error("The bound variable is used twice.");
                }
            }
        }

    }

    /**
     * @brief Get the order of bound vars. The order will include all the bound variables of the form SUM[s, FUN[i, ...]].
     * 
     * @param term 
     * @return vector<int> 
     */
    vector<int> get_order_of_bound_vars(TermPtr<int> term) {
        std::set<int> bound_vars_set;
        std::vector<int> bound_vars_order;
        std::map<int, TermPtr<int>> bound_vars_sumset;
        _iter_for_order(term, bound_vars_set, bound_vars_order, bound_vars_sumset);

        // check whether bound_vars_sumset should be included in the order
        std::vector<pair<int, TermPtr<int>>> necessary_sum_vars;
        for (const auto& [var, sumset] : bound_vars_sumset) {
            if (bound_vars_set.find(var) != bound_vars_set.end()) {
                necessary_sum_vars.push_back({var, sumset});
            }
        }

        // sort necessary sumset bound variables (according to the sum set)
        std::sort(necessary_sum_vars.begin(), necessary_sum_vars.end(), [&](const pair<int, TermPtr<int>>& a, const pair<int, TermPtr<int>>& b) {
            return *a.second < *b.second;
        });

        // append to the bound_vars_order
        for (const auto& [var, _] : necessary_sum_vars) {
            bound_vars_order.push_back(var);
        }

        return bound_vars_order;
    }

    // the structure to store the bound variable information
    struct SumSwapHead {
        int head;
        TermPtr<int> set;
        TermPtr<int> type;
    };

    TermPtr<int> _sum_swap_normalization(Kernel& kernel, TermPtr<int> term, const map<int, int>& var_to_order) {
        if (term->is_atomic()) {
            return term;
        }

        // pointer to the first inner term that is not within a chaining of sum
        TermPtr<int> inner_term = term;
        vector<SumSwapHead> sum_swap_heads;

        while (true) {
            if (inner_term->get_head() != SUM) break;

            auto& args_sum = inner_term->get_args();
            
            if (args_sum[1]->get_head() != FUN) break; 

            auto& args_fun = args_sum[1]->get_args();

            sum_swap_heads.emplace_back(SumSwapHead{args_fun[0]->get_head(), args_sum[0], args_fun[1]});

            inner_term = args_fun[2];
        }

        if (sum_swap_heads.size() == 0) {
            // no sum swap to do
            auto& args = term->get_args();
            ListArgs<int> new_args;
            for (const auto& arg : args) {
                new_args.push_back(_sum_swap_normalization(kernel, arg, var_to_order));
            }
            return create_term(term->get_head(), new_args);
        }

        else {
            // get the normolized inner term
            inner_term = _sum_swap_normalization(kernel, inner_term, var_to_order);

            // sort the sum swap heads
            std::sort(sum_swap_heads.begin(), sum_swap_heads.end(), [&](const SumSwapHead& a, const SumSwapHead& b) {
                return var_to_order.at(a.head) < var_to_order.at(b.head);
            });

            // create the from innermost to out
            for (int i = sum_swap_heads.size() - 1; i >= 0; i--) {
                auto& head = sum_swap_heads[i];
                inner_term = create_term(
                    SUM,
                    {
                        head.set,
                        create_term(
                            FUN,
                            {
                                create_term(head.head),
                                head.type,
                                inner_term
                            }
                        )
                    }
                );
            }

            return inner_term;
        }
            
    }

    TermPtr<int> sum_swap_normalization(Kernel& kernel, TermPtr<int> term) {

        // get all the bound variables
        std::set<int> bound_vars;
        _get_bound_vars(term, bound_vars);

        // iterate through the term to get the order
        std::vector<int> bound_vars_order = get_order_of_bound_vars(term);

        // create the order map
        map<int, int> var_to_order;
        for (int i = 0; i < bound_vars_order.size(); i++) {
            var_to_order[bound_vars_order[i]] = i;
        }

        return _sum_swap_normalization(kernel, term, var_to_order);
    }

    TermPtr<int> sort_modulo_bound(Kernel& kernel, TermPtr<int> term) {
        auto bound_vars = get_bound_vars(term);

        return sort_C_terms(term, c_symbols, 
            [&](TermPtr<int> a, TermPtr<int> b) {
                return comp_modulo_bound_vars(a, b, bound_vars);
            }
        );
    }


    TermPtr<int> wolfram_fullsimplify(Kernel& kernel, ualg::TermPtr<int> term) {
        using namespace astparser;
        // check whether it has the link
        auto link = kernel.get_wstp_link();
        if (!link) return term;

        auto &sig = kernel.get_sig();

        // we use this special simplification to avoid factoring
        auto ast = AST(
            "FullSimplify", {
                sig.term2ast(term), 
                AST("Rule", {
                AST("TransformationFunctions"), 
                    AST("List", {
                        AST("FunctionExpand"),
                        AST("TrigExpand"),
                        AST("PowerExpand")
                    })
                })
            });

        // auto ast = AST(
        //     "FullSimplify", {
        //         sig.term2ast(term)
        //     });

        // Call the Wolfram Engine
        wstp::ast_to_WS(link, ast);

        // Get the result
        auto res_ast = wstp::WS_to_ast(link);
        auto res_temp = sig.ast2term(res_ast);

        return res_temp;
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
        
        return create_term(MULS, args);
    }

    // a : STYPE, K : KTYPE(T) => a @ K -> SCR(a K)
    DIRACOQ_RULE_DEF(R_COMPO_SK, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeA = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeA->get_head() == STYPE and typeB->get_head() == KTYPE)) return std::nullopt;
        
        return create_term(SCR, args);
    }

    // a : STYPE, B : BTYPE(T) => a @ B -> SCR(a B)
    DIRACOQ_RULE_DEF(R_COMPO_SB, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeA = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeA->get_head() == STYPE and typeB->get_head() == BTYPE)) return std::nullopt;
        
        return create_term(SCR, args);
    }

    // a : STYPE, O : OTYPE(T1 T2) => a @ O -> SCR(a O)
    DIRACOQ_RULE_DEF(R_COMPO_SO, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeA = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeA->get_head() == STYPE and typeB->get_head() == OTYPE)) return std::nullopt;
        
        return create_term(SCR, args);
    }


    // K : KTYPE(T), a : STYPE => COMPO(K a) -> SCR(a K)
    DIRACOQ_RULE_DEF(R_COMPO_KS, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeK = kernel.calc_type(args[0]);
        auto typeA = kernel.calc_type(args[1]);

        if (!(typeK->get_head() == KTYPE and typeA->get_head() == STYPE)) return std::nullopt;
        
        return create_term(SCR, {args[1], args[0]});
    }

    // K1 : KTYPE(T1), K2 : KTYPE(T2) => COMPO(K1 K2) -> TSR(K1 K2)
    DIRACOQ_RULE_DEF(R_COMPO_KK, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeK1 = kernel.calc_type(args[0]);
        auto typeK2 = kernel.calc_type(args[1]);

        if (!(typeK1->get_head() == KTYPE and typeK2->get_head() == KTYPE)) return std::nullopt;
        
        return create_term(TSR, args);
    }

    // K : KTYPE(T1), B : BTYPE(T2) => COMPO(K B) -> OUTER(K B)
    DIRACOQ_RULE_DEF(R_COMPO_KB, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeK = kernel.calc_type(args[0]);
        auto typeB = kernel.calc_type(args[1]);

        if (!(typeK->get_head() == KTYPE and typeB->get_head() == BTYPE)) return std::nullopt;
        
        return create_term(OUTER, args);
    }


    // B : BTYPE(T), a : STYPE => COMPO(B a) -> SCR(a B)
    DIRACOQ_RULE_DEF(R_COMPO_BS, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeB = kernel.calc_type(args[0]);
        auto typeA = kernel.calc_type(args[1]);

        if (!(typeB->get_head() == BTYPE and typeA->get_head() == STYPE)) return std::nullopt;
        
        return create_term(SCR, {args[1], args[0]});
    }

    // B : BTYPE(T), K : KTYPE(T) => COMPO(B K) -> DOT(B K)
    DIRACOQ_RULE_DEF(R_COMPO_BK, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeB = kernel.calc_type(args[0]);
        auto typeK = kernel.calc_type(args[1]);

        if (!(typeB->get_head() == BTYPE and typeK->get_head() == KTYPE)) return std::nullopt;
        
        return create_term(DOT, args);
    }

    // B1 : BTYPE(T1), B2 : BTYPE(T2) => COMPO(B1 B2) -> TSR(B1 B2)
    DIRACOQ_RULE_DEF(R_COMPO_BB, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeB1 = kernel.calc_type(args[0]);
        auto typeB2 = kernel.calc_type(args[1]);

        if (!(typeB1->get_head() == BTYPE and typeB2->get_head() == BTYPE)) return std::nullopt;
        
        return create_term(TSR, args);
    }

    // B : BTYPE(T1), O : OTYPE(T1 T2) => COMPO(B O) -> MULB(B O)
    DIRACOQ_RULE_DEF(R_COMPO_BO, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeB = kernel.calc_type(args[0]);
        auto typeO = kernel.calc_type(args[1]);

        if (!(typeB->get_head() == BTYPE and typeO->get_head() == OTYPE)) return std::nullopt;
        
        return create_term(MULB, args);
    }

    // O : OTYPE(T1 T2), a : STYPE => COMPO(O a) -> SCR(a O)
    DIRACOQ_RULE_DEF(R_COMPO_OS, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeO = kernel.calc_type(args[0]);
        auto typeA = kernel.calc_type(args[1]);

        if (!(typeO->get_head() == OTYPE and typeA->get_head() == STYPE)) return std::nullopt;
        
        return create_term(SCR, {args[1], args[0]});
    }

    // O : OTYPE(T1 T2), K : KTYPE(T2) => COMPO(O K) -> MULK(O K)
    DIRACOQ_RULE_DEF(R_COMPO_OK, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeO = kernel.calc_type(args[0]);
        auto typeK = kernel.calc_type(args[1]);

        if (!(typeO->get_head() == OTYPE and typeK->get_head() == KTYPE)) return std::nullopt;
        
        return create_term(MULK, args);
    }

    // O1 : OTYPE(T1 T2), O2 : OTYPE(T2 T3) => COMPO(O1 O2) -> MULO(O1 O2)
    DIRACOQ_RULE_DEF(R_COMPO_OO, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeO1 = kernel.calc_type(args[0]);
        auto typeO2 = kernel.calc_type(args[1]);

        if (!(typeO1->get_head() == OTYPE and typeO2->get_head() == OTYPE)) return std::nullopt;
        
        return create_term(MULO, args);
    }

    // D1 : DTYPE[...], D2 : DTYPE[...] => COMPO(D1 D2) -> LDOT[D1 D2]
    DIRACOQ_RULE_DEF(R_COMPO_DD, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeD1 = kernel.calc_type(args[0]);
        auto typeD2 = kernel.calc_type(args[1]);

        if (!(typeD1->get_head() == DTYPE and typeD2->get_head() == DTYPE)) return std::nullopt;
        
        return create_term(LDOT, args);
    }

    // f : T1 -> T2 => COMPO(f a) -> APPLY(f a)
    DIRACOQ_RULE_DEF(R_COMPO_ARROW, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeF = kernel.calc_type(args[0]);

        if (!(typeF->get_head() == ARROW)) return std::nullopt;
        
        return create_term(APPLY, args);
    }

    // f : FORALL(x T1) => COMPO(f a) -> APPLY(f a)
    DIRACOQ_RULE_DEF(R_COMPO_FORALL, kernel, term) {
        MATCH_HEAD(term, COMPO, args)
        auto typeF = kernel.calc_type(args[0]);

        if (!(typeF->get_head() == FORALL)) return std::nullopt;
        
        return create_term(APPLY, args);
    }

    // T1 : INDEX, T2 : INDEX => STAR(T1 T2) -> PROD(T1 T2)
    DIRACOQ_RULE_DEF(R_STAR_PROD, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto typeA = kernel.calc_type(args[0]);

        if (!(typeA->get_head() == INDEX)) return std::nullopt;

        return create_term(PROD, args);
    }

    // an : SType => STAR(a1 ... an) -> MULS(a1 ... an)
    DIRACOQ_RULE_DEF(R_STAR_MULS, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto type1 = kernel.calc_type(args[0]);

        if (!(type1->get_head() == STYPE)) return std::nullopt;

        return create_term(MULS, args);
    }

    // O1 : OType(T1 T2), O2 : OType(T3 T3) => STAR(O1 O2) -> TSR(O1 O2)
    DIRACOQ_RULE_DEF(R_STAR_TSRO, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto typeA = kernel.calc_type(args[0]);
        
        if (!(typeA->get_head() == OTYPE)) return std::nullopt;

        return create_term(TSR, args);
    }

    // S1 : SET(T1), S2 : SET(T2) => STAR(S1 S2) -> CATPROD(S1 S2)
    DIRACOQ_RULE_DEF(R_STAR_CATPROD, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto typeA = kernel.calc_type(args[0]);
        
        if (!(typeA->get_head() == SET)) return std::nullopt;

        return create_term(CATPROD, args);
    }

    // D1 : DTYPE[...], D2 : DTYPE[...] => STAR(D1 D2) -> LTSR[D1 D2]
    DIRACOQ_RULE_DEF(R_STAR_LTSR, kernel, term) {
        MATCH_HEAD(term, STAR, args)
        auto typeA = kernel.calc_type(args[0]);

        if (!(typeA->get_head() == DTYPE)) return std::nullopt;

        return create_term(LTSR, args);
    }

    // an : STYPE => ADDG(a1 ... an) -> ADDS(a1 ... an)
    DIRACOQ_RULE_DEF(R_ADDG_ADDS, kernel, term) {
        MATCH_HEAD(term, ADDG, args)
        auto type1 = kernel.calc_type(args[0]);

        if (!(type1->get_head() == STYPE)) return std::nullopt;

        return create_term(ADDS, args);
    }

    // an : KTYPE(T), BTYPE(T) or OTYPE(T1 T2) => ADDG(a1 ... an) -> ADD(a1 ... an)
    DIRACOQ_RULE_DEF(R_ADDG_ADD, kernel, term) {
        MATCH_HEAD(term, ADDG, args)
        auto type1 = kernel.calc_type(args[0]);

        if (!(type1->get_head() == KTYPE or type1->get_head() == BTYPE or type1->get_head() == OTYPE)) return std::nullopt;

        return create_term(ADD, args);
    }

    // S : SET(T) => SSUM(i S e) -> SUM(S FUN(i BASIS(T) e))
    DIRACOQ_RULE_DEF(R_SSUM, kernel, term) {
        MATCH_HEAD(term, SSUM, args)
        auto typeS = kernel.calc_type(args[1]);

        return create_term(SUM, {
            args[1],
            create_term(FUN, {
                args[0],
                create_term(BASIS, {typeS->get_args()[0]}),
                args[2]
            })
        });
    }

    //////////////////////////////////////////////////////////////
    // Main rewriting rules

    DIRACOQ_RULE_DEF(R_BETA_ARROW, kernel, term) {
        MATCH_HEAD(term, APPLY, args)
        MATCH_HEAD(args[0], FUN, fun_args)
        if (fun_args.size() != 3) return std::nullopt;
        return subst(kernel.get_sig(), fun_args[2], fun_args[0]->get_head(), args[1]);
    }

    DIRACOQ_RULE_DEF(R_BETA_INDEX, kernel, term) {
        MATCH_HEAD(term, APPLY, args)
        MATCH_HEAD(args[0], IDX, idx_args)
        if (idx_args.size() != 2) return std::nullopt;
        return subst(kernel.get_sig(), idx_args[1], idx_args[0]->get_head(), args[1]);
    }

    DIRACOQ_RULE_DEF(R_DELTA, kernel, term) {
        auto find_res = kernel.find_in_env(term->get_head());
        if (find_res != std::nullopt and find_res->is_def()) {
            // Note that the bound variable renaming is applied
            auto renamed_res = bound_variable_rename(kernel, find_res->def.value());
            return renamed_res;
        }
        return std::nullopt;
    }

    // //////////////// Flattening AC symbols
    DIRACOQ_RULE_DEF(R_FLATTEN, kernel, term) {
        auto res = flatten<int>(term, a_symbols);
        if (*res != *term) {
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
        auto zero_term = create_term(ZERO);

        MATCH_HEAD(term, ADDS, args_ADDS_a_0)
        
        ListArgs<int> new_args;
        for (const auto& arg : args_ADDS_a_0) {
            if (*arg == *zero_term) {
                continue;
            }
            new_args.push_back(arg);
        }
        if (new_args.empty()) {
            new_args.push_back(zero_term);
        }

        if (new_args.size() == args_ADDS_a_0.size()) return std::nullopt;

        return create_term(ADDS, std::move(new_args));
    }

    // MULS(a 0) -> 0
    DIRACOQ_RULE_DEF(R_MULS0, kernel, term) {
        auto zero_term = create_term(ZERO);

        MATCH_HEAD(term, MULS, args_MULS_a_0)

        for (const auto& arg : args_MULS_a_0) {
            if (*arg == *zero_term) {
                return zero_term;
            }
        }

        return std::nullopt;
    }

    // MULS(a 1) -> a
    DIRACOQ_RULE_DEF(R_MULS1, kernel, term) {

        auto one_term = create_term(ONE);

        MATCH_HEAD(term, MULS, args_MULS_a_1)
        
        ListArgs<int> new_args;
        for (const auto& arg : args_MULS_a_1) {
            if (*arg == *one_term) {
                continue;
            }
            new_args.push_back(arg);
        }
        if (new_args.empty()) {
            new_args.push_back(one_term);
        }
        
        if (new_args.size() == args_MULS_a_1.size()) return std::nullopt;

        return create_term(MULS, std::move(new_args));
    }


    // MULS(a ADDS(b c)) -> ADDS(MULS(a b) MULS(a c))
    DIRACOQ_RULE_DEF(R_MULS2, kernel, term) {

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
                    newargs_ADDS_MULS.push_back(create_term(MULS, std::move(newargs_MULS)));
                }

                return create_term(ADDS, std::move(newargs_ADDS_MULS));
            }
        }

        return std::nullopt;
    }

    // CONJ(0) -> 0
    DIRACOQ_RULE_DEF(R_CONJ0, kernel, term) {

        auto zero_term = create_term(ZERO);
        auto CONJ_0_term = create_term(CONJ, {zero_term});

        if (*term == *CONJ_0_term) {
            return zero_term;
        }

        return std::nullopt;
    }

    // CONJ(1) -> 1
    DIRACOQ_RULE_DEF(R_CONJ1, kernel, term) {

        auto one_term = create_term(ONE);
        auto CONJ_1_term = create_term(CONJ, {one_term});

        if (*term == *CONJ_1_term) {
            return one_term;
        }

        return std::nullopt;
    }

    // CONJ(ADDS(a b)) -> ADDS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ2, kernel, term) {

        MATCH_HEAD(term, CONJ, args_CONJ_ADDS_a_b)

        MATCH_HEAD(args_CONJ_ADDS_a_b[0], ADDS, args_ADDS_a_b)
        
        ListArgs<int> newargs_ADDS_CONJ;
        for (const auto& arg : args_ADDS_a_b) {
            newargs_ADDS_CONJ.push_back(create_term(CONJ, {arg}));
        }
        return create_term(ADDS, std::move(newargs_ADDS_CONJ));
    }

    // CONJ(MULS(a b)) -> MULS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ3, kernel, term) {

        MATCH_HEAD(term, CONJ, args_CONJ_MULS_a_b)

        MATCH_HEAD(args_CONJ_MULS_a_b[0], MULS, args_MULS_a_b)
            
        ListArgs<int> newargs_MULS_CONJ;
        for (const auto& arg : args_MULS_a_b) {
            newargs_MULS_CONJ.push_back(create_term(CONJ, {arg}));
        }
        return create_term(MULS, std::move(newargs_MULS_CONJ));
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

        MATCH_HEAD(term, CONJ, args_CONJ_DOT_B_K)
        
        MATCH_HEAD(args_CONJ_DOT_B_K[0], DOT, args_DOT_B_K)

        return create_term(DOT,
            {
                create_term(ADJ, {args_DOT_B_K[1]}),
                create_term(ADJ, {args_DOT_B_K[0]})
            }
        );
    }

    // DOT(0B(sigma) K) -> 0
    DIRACOQ_RULE_DEF(R_DOT0, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_0B_sigma_K)
        
        if (args_DOT_0B_sigma_K[0]->get_head() != ZEROB) return std::nullopt;
        
        return create_term(ZERO);
    }

    // DOT(B 0K(sigma)) -> 0
    DIRACOQ_RULE_DEF(R_DOT1, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_B_0K_sigma)

        if (args_DOT_B_0K_sigma[1]->get_head() != ZEROK) return std::nullopt;
        
        return create_term(ZERO);
    }

    // DOT(SCR(a B) K) -> MULS(a DOT(B K))
    DIRACOQ_RULE_DEF(R_DOT2, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_SCR_a_B_K)
        
        MATCH_HEAD(args_DOT_SCR_a_B_K[0], SCR, args_SCR_a_B)
            
        return create_term(MULS, 
            {
                args_SCR_a_B[0], 
                create_term(DOT, {args_SCR_a_B[1], args_DOT_SCR_a_B_K[1]})
            }
        );
    }

    // DOT(B SCR(a K)) -> MULS(a DOT(B K))
    DIRACOQ_RULE_DEF(R_DOT3, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_B_SCR_a_K)
        
        MATCH_HEAD(args_DOT_B_SCR_a_K[1], SCR, args_SCR_a_K)
            
        return create_term(MULS, 
            {
                args_SCR_a_K[0], 
                create_term(DOT, {args_DOT_B_SCR_a_K[0], args_SCR_a_K[1]})
            }
        );
    }

    // DOT(ADD(B1 ... Bn) K) -> ADDS(DOT(B1 K) ... DOT(Bn K))
    DIRACOQ_RULE_DEF(R_DOT4, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_ADD_B1_Bn_K)
        
        MATCH_HEAD(args_DOT_ADD_B1_Bn_K[0], ADD, args_ADD_B1_Bn)
            
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_B1_Bn) {
            new_args.push_back(create_term(DOT, {arg, args_DOT_ADD_B1_Bn_K[1]}));
        }
        return create_term(ADDS, std::move(new_args));
    }

    // DOT(B ADD(K1 ... Kn)) -> ADDS(DOT(B K1) ... DOT(B Kn))
    DIRACOQ_RULE_DEF(R_DOT5, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_B_ADD_K1_Kn)
        
        MATCH_HEAD(args_DOT_B_ADD_K1_Kn[1], ADD, args_ADD_K1_Kn)
                
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_K1_Kn) {
            new_args.push_back(create_term(DOT, {args_DOT_B_ADD_K1_Kn[0], arg}));
        }
        return create_term(ADDS, std::move(new_args));
    }

    // DOT(BRA(s) KET(t)) -> DELTA(s t)
    DIRACOQ_RULE_DEF(R_DOT6, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_BRA_s_KET_t)
        
        MATCH_HEAD(args_DOT_BRA_s_KET_t[0], BRA, args_BRA_s)
            
        MATCH_HEAD(args_DOT_BRA_s_KET_t[1], KET, args_KET_t)
        
        return create_term(DELTA, {args_BRA_s[0], args_KET_t[0]});
    }

    // DOT(TSR(B1 B2) KET(PAIR(s t))) -> MULS(DOT(B1 KET(s)) DOT(B2 KET(t)))
    DIRACOQ_RULE_DEF(R_DOT7, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_TSR_B1_B2_KET_PAIR_s_t)

        MATCH_HEAD(args_DOT_TSR_B1_B2_KET_PAIR_s_t[0], TSR, args_TSR_B1_B2)
        
        MATCH_HEAD(args_DOT_TSR_B1_B2_KET_PAIR_s_t[1], KET, args_KET_PAIR_s_t)

        MATCH_HEAD(args_KET_PAIR_s_t[0], PAIR, args_PAIR_s_t)

        return create_term(MULS, 
            {
                create_term(DOT, {args_TSR_B1_B2[0], create_term(KET, {args_PAIR_s_t[0]})}),
                create_term(DOT, {args_TSR_B1_B2[1], create_term(KET, {args_PAIR_s_t[1]})})
            }
        );
    }

    // DOT(BRA(PAIR(s t)) TSR(K1 K2)) -> MULS(DOT(BRA(s) K1) DOT(BRA(t) K2))
    DIRACOQ_RULE_DEF(R_DOT8, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_BRA_PAIR_s_t_TSR_K1_K2)
        
        MATCH_HEAD(args_DOT_BRA_PAIR_s_t_TSR_K1_K2[0], BRA, args_BRA_PAIR_s_t)
            
        MATCH_HEAD(args_BRA_PAIR_s_t[0], PAIR, args_PAIR_s_t)
                
        MATCH_HEAD(args_DOT_BRA_PAIR_s_t_TSR_K1_K2[1], TSR, args_TSR_K1_K2)
                    
        return create_term(MULS, 
            {
                create_term(DOT, {create_term(BRA, {args_PAIR_s_t[0]}), args_TSR_K1_K2[0]}),
                create_term(DOT, {create_term(BRA, {args_PAIR_s_t[1]}), args_TSR_K1_K2[1]})
            }
        );
    }

    // DOT(TSR(B1 B2) TSR(K1 K2)) -> MULS(DOT(B1 K1) DOT(B2 K2))
    DIRACOQ_RULE_DEF(R_DOT9, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_TSR_B1_B2_TSR_K1_K2)
        
        MATCH_HEAD(args_DOT_TSR_B1_B2_TSR_K1_K2[0], TSR, args_TSR_B1_B2)
            
        MATCH_HEAD(args_DOT_TSR_B1_B2_TSR_K1_K2[1], TSR, args_TSR_K1_K2)
                
        return create_term(MULS, 
            {
                create_term(DOT, {args_TSR_B1_B2[0], args_TSR_K1_K2[0]}),
                create_term(DOT, {args_TSR_B1_B2[1], args_TSR_K1_K2[1]})
            }
        );
    }

    // DOT(MULB(B O) K) -> DOT(B MULK(O K))
    DIRACOQ_RULE_DEF(R_DOT10, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_MULB_B_O_K)
        
        MATCH_HEAD(args_DOT_MULB_B_O_K[0], MULB, args_MULB_B_O)
            
        return create_term(DOT, 
            {
                args_MULB_B_O[0], 
                create_term(MULK, {args_MULB_B_O[1], args_DOT_MULB_B_O_K[1]})
            }
        );
    }

    // DOT(BRA(PAIR(s t)) MULK(TSR(O1 O2) K)) -> DOT(TSR(MULB(BRA(s) O1) MULB(BRA(t) O2)) K)
    DIRACOQ_RULE_DEF(R_DOT11, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_BRA_PAIR_s_t_MULK_TSR_O1_O2_K)

        MATCH_HEAD(args_DOT_BRA_PAIR_s_t_MULK_TSR_O1_O2_K[0], BRA, args_BRA_PAIR_s_t)
        
        MATCH_HEAD(args_BRA_PAIR_s_t[0], PAIR, args_PAIR_s_t)
        
        MATCH_HEAD(args_DOT_BRA_PAIR_s_t_MULK_TSR_O1_O2_K[1], MULK, args_MULK_TSR_O1_O2_K)

        MATCH_HEAD(args_MULK_TSR_O1_O2_K[0], TSR, args_TSR_O1_O2)

        return create_term(DOT, 
            {
                create_term(TSR, 
                    {
                        create_term(MULB, {create_term(BRA, {args_PAIR_s_t[0]}), args_TSR_O1_O2[0]}),
                        create_term(MULB, {create_term(BRA, {args_PAIR_s_t[1]}), args_TSR_O1_O2[1]})
                    }
                ),
                args_MULK_TSR_O1_O2_K[1]
            }
        );                
    }

    // DOT(TSR(B1 B2) MULK(TSR(O1 O2) K)) -> DOT(TSR(MULB(B1 O1) MULB(B2 O2)) K)
    DIRACOQ_RULE_DEF(R_DOT12, kernel, term) {

        MATCH_HEAD(term, DOT, args_DOT_TSR_B1_B2_MULK_TSR_O1_O2_K)
        
        MATCH_HEAD(args_DOT_TSR_B1_B2_MULK_TSR_O1_O2_K[0], TSR, args_TSR_B1_B2)

        MATCH_HEAD(args_DOT_TSR_B1_B2_MULK_TSR_O1_O2_K[1], MULK, args_MULK_TSR_O1_O2_K)

        MATCH_HEAD(args_MULK_TSR_O1_O2_K[0], TSR, args_TSR_O1_O2)

        return create_term(DOT, 
            {
                create_term(TSR, 
                    {
                        create_term(MULB, {args_TSR_B1_B2[0], args_TSR_O1_O2[0]}),
                        create_term(MULB, {args_TSR_B1_B2[1], args_TSR_O1_O2[1]})
                    }
                ),
                args_MULK_TSR_O1_O2_K[1]
            }
        );                
    }

    // DELTA(a a) -> 1
    DIRACOQ_RULE_DEF(R_DELTA0, kernel, term) {

        MATCH_HEAD(term, DELTA, args_DELTA_a_a)

        if (*args_DELTA_a_a[1] != *args_DELTA_a_a[0]) return std::nullopt;

        return create_term(ONE);
    }

    // DELTA(PAIR(a b) PAIR(c d)) -> MULS(DELTA(a c) DELTA(b d))
    DIRACOQ_RULE_DEF(R_DELTA1, kernel, term) {

        MATCH_HEAD(term, DELTA, args_DELTA_PAIR_a_b_PAIR_c_d)
        
        MATCH_HEAD(args_DELTA_PAIR_a_b_PAIR_c_d[0], PAIR, args_PAIR_a_b)
        
        MATCH_HEAD(args_DELTA_PAIR_a_b_PAIR_c_d[1], PAIR, args_PAIR_c_d)

        return create_term(MULS, 
            {
                create_term(DELTA, {args_PAIR_a_b[0], args_PAIR_c_d[0]}),
                create_term(DELTA, {args_PAIR_a_b[1], args_PAIR_c_d[1]})
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

        MATCH_HEAD(term, SCR, args_SCR_a_SCR_b_X)
        
        MATCH_HEAD(args_SCR_a_SCR_b_X[1], SCR, args_SCR_b_X)

        return create_term(SCR, 
            {
                create_term(MULS, {args_SCR_a_SCR_b_X[0], args_SCR_b_X[0]}),
                args_SCR_b_X[1]
            }
        );
    }

    // SCR(a ADD(X1 ... Xn)) -> ADD(SCR(a X1) ... SCR(a Xn))
    DIRACOQ_RULE_DEF(R_SCR2, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_a_ADD_X1_Xn)
        
        MATCH_HEAD(args_SCR_a_ADD_X1_Xn[1], ADD, args_ADD_X1_Xn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(create_term(SCR, {args_SCR_a_ADD_X1_Xn[0], arg}));
        }
        return create_term(ADD, std::move(new_args));
    }
    
    // K : KTYPE(T) => SCR(0 K) -> 0K(T)
    DIRACOQ_RULE_DEF(R_SCRK0, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_0_K)
        
        // Check the typing of K
        auto type_K = kernel.calc_type(args_SCR_0_K[1]);

        MATCH_HEAD(type_K, KTYPE, args_KType_T)

        if (args_SCR_0_K[0]->get_head() != ZERO) return std::nullopt;

        return create_term(ZEROK, {args_KType_T[0]});
    }

    // SCR(a 0K(T)) -> 0K(T)
    DIRACOQ_RULE_DEF(R_SCRK1, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_a_0K_T)
        
        if (args_SCR_a_0K_T[1]->get_head() != ZEROK) return std::nullopt;

        return args_SCR_a_0K_T[1];
    }

    // B : BTYPE(T) => SCR(0 B) -> 0B(T)
    DIRACOQ_RULE_DEF(R_SCRB0, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_0_B)
        
        // Check the typing of B
        auto type_B = kernel.calc_type(args_SCR_0_B[1]);

        MATCH_HEAD(type_B, BTYPE, args_BType_T)

        if (args_SCR_0_B[0]->get_head() != ZERO) return std::nullopt;

        return create_term(ZEROB, {args_BType_T[0]});
    }

    // SCR(a 0B(T)) -> 0B(T)
    DIRACOQ_RULE_DEF(R_SCRB1, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_a_0B_T)
        
        if (args_SCR_a_0B_T[1]->get_head() != ZEROB) return std::nullopt;

        return args_SCR_a_0B_T[1];
    }

    // O : OTYPE(T1 T2) => SCR(0 O) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_SCRO0, kernel, term) {

        MATCH_HEAD(term, SCR, args_SCR_0_O)
        
        // Check the typing of O
        auto type_O = kernel.calc_type(args_SCR_0_O[1]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        if (args_SCR_0_O[0]->get_head() != ZERO) return std::nullopt;

        return create_term(ZEROO, {args_OType_T1_T2[0], args_OType_T1_T2[1]});
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

        MATCH_HEAD(term, ADD, args_ADD_Y1_X_X_Yn)
        
        if (args_ADD_Y1_X_X_Yn.size() < 2) return std::nullopt;

        for (int i = 0; i < args_ADD_Y1_X_X_Yn.size() - 1; i++) {
            for (int j = i + 1; j < args_ADD_Y1_X_X_Yn.size(); j++) {
                if (*args_ADD_Y1_X_X_Yn[i] == *args_ADD_Y1_X_X_Yn[j]) {
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
                        create_term(SCR, 
                            {
                                create_term(ADDS, {create_term(ONE, {}), create_term(ONE, {})}),
                                args_ADD_Y1_X_X_Yn[i]
                            }
                        )
                    );
                    return create_term(ADD, std::move(new_args));
                }
            }
        }
        return std::nullopt;
    }

    // ADD(Y1 ... X ... SCR(a X) ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(1 a) X))
    DIRACOQ_RULE_DEF(R_ADD1, kernel, term) {

        MATCH_HEAD(term, ADD, args_ADD_Y1_X_X_Yn)
        
        if (args_ADD_Y1_X_X_Yn.size() < 2) return std::nullopt;

        for (int i = 0; i < args_ADD_Y1_X_X_Yn.size() - 1; i++) {
            for (int j = i + 1; j < args_ADD_Y1_X_X_Yn.size(); j++) {

                if (args_ADD_Y1_X_X_Yn[j]->get_head() == SCR) {
                    auto& args_SCR_a_X = args_ADD_Y1_X_X_Yn[j]->get_args();
                    if (*args_SCR_a_X[1] == *args_ADD_Y1_X_X_Yn[i]) {
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
                            create_term(SCR, 
                                {
                                    create_term(ADDS, {create_term(ONE, {}), args_SCR_a_X[0]}),
                                    args_ADD_Y1_X_X_Yn[i]
                                }
                            )
                        );
                        return create_term(ADD, std::move(new_args));
                    }
                }
            }
        }
        return std::nullopt;
    }


    // ADD(Y1 ... SCR(a X) ... X ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(a 1) X))
    DIRACOQ_RULE_DEF(R_ADD2, kernel, term) {

        MATCH_HEAD(term, ADD, args_ADD_Y1_X_X_Yn)
        
        if (args_ADD_Y1_X_X_Yn.size() < 2) return std::nullopt;

        for (int i = 0; i < args_ADD_Y1_X_X_Yn.size() - 1; i++) {

            if (args_ADD_Y1_X_X_Yn[i]->get_head() == SCR) {
                auto& args_SCR_a_X = args_ADD_Y1_X_X_Yn[i]->get_args();

                for (int j = i + 1; j < args_ADD_Y1_X_X_Yn.size(); j++) {
                    if (*args_ADD_Y1_X_X_Yn[j] == *args_SCR_a_X[1]) {
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
                            create_term(SCR, 
                                {
                                    create_term(ADDS, {args_SCR_a_X[0], create_term(ONE, {})}),
                                    args_SCR_a_X[1]
                                }
                            )
                        );
                        return create_term(ADD, std::move(new_args));
                    }
                }
            }
        }
        return std::nullopt;
    }

    // ADD(Y1 ... SCR(a X) ... SCR(b X) ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(a b) X))
    DIRACOQ_RULE_DEF(R_ADD3, kernel, term) {

        MATCH_HEAD(term, ADD, args_ADD_Y1_X_X_Yn)
        
        if (args_ADD_Y1_X_X_Yn.size() < 2) return std::nullopt;

        for (int i = 0; i < args_ADD_Y1_X_X_Yn.size() - 1; i++) {
            
            if (args_ADD_Y1_X_X_Yn[i]->get_head() == SCR) {
                auto& args_SCR_a_X = args_ADD_Y1_X_X_Yn[i]->get_args();
            
                for (int j = i + 1; j < args_ADD_Y1_X_X_Yn.size(); j++) {
                    
                    if (args_ADD_Y1_X_X_Yn[j]->get_head() == SCR) {
                        auto& args_SCR_b_X = args_ADD_Y1_X_X_Yn[j]->get_args();

                        if (*args_SCR_a_X[1] == *args_SCR_b_X[1]) {

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
                                create_term(SCR, 
                                    {
                                        create_term(ADDS, {args_SCR_a_X[0], args_SCR_b_X[0]}),
                                        args_SCR_a_X[1]
                                    }
                                )
                            );
                            return create_term(ADD, std::move(new_args));
                        }
                    }
                }
            }
        }
        return std::nullopt;
    }

    // ADD(K1 ... 0K(T) ... Kn) -> ADD(K1 ... Kn)
    DIRACOQ_RULE_DEF(R_ADDK0, kernel, term) {

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

        return create_term(ADD, std::move(new_args));
    }

    // ADD(B1 ... 0B(T) ... Bn) -> ADD(B1 ... Bn)
    DIRACOQ_RULE_DEF(R_ADDB0, kernel, term) {

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

        return create_term(ADD, std::move(new_args));
    }

    // ADD(O1 ... 0O(T1 T2) ... On) -> ADD(O1 ... On)
    DIRACOQ_RULE_DEF(R_ADDO0, kernel, term) {

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

        return create_term(ADD, std::move(new_args));
    }

    // ADJ(ADJ(X)) -> X
    DIRACOQ_RULE_DEF(R_ADJ0, kernel, term) {

        MATCH_HEAD(term, ADJ, args_ADJ_ADJ_X)
        
        MATCH_HEAD(args_ADJ_ADJ_X[0], ADJ, args_ADJ_X)
        
        return args_ADJ_X[0];
    }

    // ADJ(SCR(a X)) -> SCR(CONJ(a) ADJ(X))
    DIRACOQ_RULE_DEF(R_ADJ1, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_SCR_a_X)
        
        MATCH_HEAD(args_ADJ_SCR_a_X[0], SCR, args_SCR_a_X)
        
        return create_term(SCR, 
            {
                create_term(CONJ, {args_SCR_a_X[0]}),
                create_term(ADJ, {args_SCR_a_X[1]})
            }
        );
    }

    // ADJ(ADD(X1 ... Xn)) -> ADD(ADJ(X1) ... ADJ(Xn))
    DIRACOQ_RULE_DEF(R_ADJ2, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_ADD_X1_Xn)
        
        MATCH_HEAD(args_ADJ_ADD_X1_Xn[0], ADD, args_ADD_X1_Xn)
        
        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(create_term(ADJ, {arg}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // ADJ(TSR(X Y)) -> TSR(ADJ(X) ADJ(Y))
    DIRACOQ_RULE_DEF(R_ADJ3, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_TSR_X_Y)
        
        MATCH_HEAD(args_ADJ_TSR_X_Y[0], TSR, args_TSR_X_Y)
        
        return create_term(TSR, 
            {
                create_term(ADJ, {args_TSR_X_Y[0]}),
                create_term(ADJ, {args_TSR_X_Y[1]})
            }
        );
    }

    // ADJ(0B(T)) -> 0K(T)
    DIRACOQ_RULE_DEF(R_ADJK0, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_0B_T)
        
        MATCH_HEAD(args_ADJ_0B_T[0], ZEROB, args_ZEROB_T)

        return create_term(ZEROK, {args_ZEROB_T[0]});
    }

    // ADJ(BRA(t)) -> KET(t)
    DIRACOQ_RULE_DEF(R_ADJK1, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_BRA_t)
        
        MATCH_HEAD(args_ADJ_BRA_t[0], BRA, args_BRA_t)

        return create_term(KET, {args_BRA_t[0]});
    }

    // ADJ(MULB(B O)) -> MULK(ADJ(O) ADJ(B))
    DIRACOQ_RULE_DEF(R_ADJK2, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_MULB_B_O)

        MATCH_HEAD(args_ADJ_MULB_B_O[0], MULB, args_MULB_B_O)

        return create_term(MULK, 
            {
                create_term(ADJ, {args_MULB_B_O[1]}),
                create_term(ADJ, {args_MULB_B_O[0]})
            }
        );
    }


    // ADJ(0K(T)) -> 0B(T)
    DIRACOQ_RULE_DEF(R_ADJB0, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_0K_T)
        
        MATCH_HEAD(args_ADJ_0K_T[0], ZEROK, args_ZEROK_T)

        return create_term(ZEROB, {args_ZEROK_T[0]});
    }

    // ADJ(KET(t)) -> BRA(t)
    DIRACOQ_RULE_DEF(R_ADJB1, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_KET_t)
        
        MATCH_HEAD(args_ADJ_KET_t[0], KET, args_KET_t)

        return create_term(BRA, {args_KET_t[0]});
    }

    // ADJ(MULK(O K)) -> MULB(ADJ(K) ADJ(O))
    DIRACOQ_RULE_DEF(R_ADJB2, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_MULK_O_K)

        MATCH_HEAD(args_ADJ_MULK_O_K[0], MULK, args_MULK_O_K)

        return create_term(MULB, 
            {
                create_term(ADJ, {args_MULK_O_K[1]}),
                create_term(ADJ, {args_MULK_O_K[0]})
            }
        );
    }


    // ADJ(0O(T1 T2)) -> 0O(T2 T1)
    DIRACOQ_RULE_DEF(R_ADJO0, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_0O_T1_T2)
        
        MATCH_HEAD(args_ADJ_0O_T1_T2[0], ZEROO, args_ZEROO_T1_T2)

        return create_term(ZEROO, {args_ZEROO_T1_T2[1], args_ZEROO_T1_T2[0]});
    }

    // ADJ(1O(T)) -> 1O(T)
    DIRACOQ_RULE_DEF(R_ADJO1, kernel, term) {

        MATCH_HEAD(term, ADJ, args_ADJ_1O_T)
        
        if (args_ADJ_1O_T[0]->get_head() != ONEO) return std::nullopt;

        return args_ADJ_1O_T[0];
    }

    // ADJ(OUTER(K B)) -> OUTER(ADJ(B) ADJ(K))
    DIRACOQ_RULE_DEF(R_ADJO2, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_OUTER_K_B)

        MATCH_HEAD(args_ADJ_OUTER_K_B[0], OUTER, args_OUTER_K_B)

        return create_term(OUTER, 
            {
                create_term(ADJ, {args_OUTER_K_B[1]}),
                create_term(ADJ, {args_OUTER_K_B[0]})
            }
        );
    }

    // ADJ(MULO(O1 O2)) -> MULO(ADJ(O2) ADJ(O1))
    DIRACOQ_RULE_DEF(R_ADJO3, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_MULO_O1_O2)

        MATCH_HEAD(args_ADJ_MULO_O1_O2[0], MULO, args_MULO_O1_O2)

        return create_term(MULO, 
            {
                create_term(ADJ, {args_MULO_O1_O2[1]}),
                create_term(ADJ, {args_MULO_O1_O2[0]})
            }
        );
    }

    // TSR(SCR(a X1) X2) -> SCR(a TSR(X1 X2))
    DIRACOQ_RULE_DEF(R_TSR0, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_SCR_a_X1_X2)

        MATCH_HEAD(args_TSR_SCR_a_X1_X2[0], SCR, args_SCR_a_X1)

        return create_term(SCR, 
            {
                args_SCR_a_X1[0],
                create_term(TSR, {args_SCR_a_X1[1], args_TSR_SCR_a_X1_X2[1]})
            }
        );
    }

    // TSR(X1 SCR(a X2)) -> SCR(a TSR(X1 X2))
    DIRACOQ_RULE_DEF(R_TSR1, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_X1_SCR_a_X2)

        MATCH_HEAD(args_TSR_X1_SCR_a_X2[1], SCR, args_SCR_a_X2)

        return create_term(SCR, 
            {
                args_SCR_a_X2[0],
                create_term(TSR, {args_TSR_X1_SCR_a_X2[0], args_SCR_a_X2[1]})
            }
        );
    }

    // TSR(ADD(X1 ... Xn) Y) -> ADD(TSR(X1 Y) ... TSR(Xn Y))
    DIRACOQ_RULE_DEF(R_TSR2, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_ADD_X1_Xn_Y)

        MATCH_HEAD(args_TSR_ADD_X1_Xn_Y[0], ADD, args_ADD_X1_Xn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(create_term(TSR, {arg, args_TSR_ADD_X1_Xn_Y[1]}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // TSR(Y ADD(X1 ... Xn)) -> ADD(TSR(Y X1) ... TSR(Y Xn))
    DIRACOQ_RULE_DEF(R_TSR3, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_Y_ADD_X1_Xn)

        MATCH_HEAD(args_TSR_Y_ADD_X1_Xn[1], ADD, args_ADD_X1_Xn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(create_term(TSR, {args_TSR_Y_ADD_X1_Xn[0], arg}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // K : KTYPE(T2) => TSR(0K(T1) K) -> 0K(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRK0, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_0K_T1_K)

        MATCH_HEAD(args_TSR_0K_T1_K[0], ZEROK, args_ZEROK_T1)

        // Check the typing of K
        auto type_K = kernel.calc_type(args_TSR_0K_T1_K[1]);

        MATCH_HEAD(type_K, KTYPE, args_KType_T2)

        return create_term(ZEROK, {create_term(PROD, {args_ZEROK_T1[0], args_KType_T2[0]})});
    }

    // K : KTYPE(T1) => TSR(K 0K(T2)) -> 0K(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRK1, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_K_0K_T2)

        MATCH_HEAD(args_TSR_K_0K_T2[1], ZEROK, args_ZEROK_T2)

        // Check the typing of K
        auto type_K = kernel.calc_type(args_TSR_K_0K_T2[0]);

        MATCH_HEAD(type_K, KTYPE, args_KType_T1)

        return create_term(ZEROK, {create_term(PROD, {args_KType_T1[0], args_ZEROK_T2[0]})});
    }

    // TSR(KET(s) KET(t)) -> KET(PAIR(s t))
    DIRACOQ_RULE_DEF(R_TSRK2, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_KET_s_KET_t)

        MATCH_HEAD(args_TSR_KET_s_KET_t[0], KET, args_KET_s)

        MATCH_HEAD(args_TSR_KET_s_KET_t[1], KET, args_KET_t)

        return create_term(KET, {create_term(PAIR, {args_KET_s[0], args_KET_t[0]})});
    }

    // B : BTYPE(T2) => TSR(0B(T1) B) -> 0B(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRB0, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_0B_T1_B)

        MATCH_HEAD(args_TSR_0B_T1_B[0], ZEROB, args_ZEROB_T1)

        // Check the typing of B
        auto type_B = kernel.calc_type(args_TSR_0B_T1_B[1]);

        MATCH_HEAD(type_B, BTYPE, args_BType_T2)

        return create_term(ZEROB, {create_term(PROD, {args_ZEROB_T1[0], args_BType_T2[0]})});
    }

    // B : BTYPE(T1) => TSR(B 0B(T2)) -> 0B(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRB1, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_B_0B_T2)

        MATCH_HEAD(args_TSR_B_0B_T2[1], ZEROB, args_ZEROB_T2)

        // Check the typing of B
        auto type_B = kernel.calc_type(args_TSR_B_0B_T2[0]);

        MATCH_HEAD(type_B, BTYPE, args_BType_T1)

        return create_term(ZEROB, {create_term(PROD, {args_BType_T1[0], args_ZEROB_T2[0]})});
    }
    
    // TSR(BRA(s) BRA(t)) -> BRA(PAIR(s t))
    DIRACOQ_RULE_DEF(R_TSRB2, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_BRA_s_BRA_t)

        MATCH_HEAD(args_TSR_BRA_s_BRA_t[0], BRA, args_BRA_s)

        MATCH_HEAD(args_TSR_BRA_s_BRA_t[1], BRA, args_BRA_t)

        return create_term(BRA, {create_term(PAIR, {args_BRA_s[0], args_BRA_t[0]})});
    }

    // O : OTYPE(T3 T4) => TSR(0O(T1 T2) O) -> 0O(PROD(T1 T3) PROD(T2 T4))
    DIRACOQ_RULE_DEF(R_TSRO0, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_0O_T1_T2_O)

        MATCH_HEAD(args_TSR_0O_T1_T2_O[0], ZEROO, args_ZEROO_T1_T2)

        // Check the typing of O
        auto type_O = kernel.calc_type(args_TSR_0O_T1_T2_O[1]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T3_T4)

        return create_term(ZEROO, 
            {
                create_term(PROD, {args_ZEROO_T1_T2[0], args_OType_T3_T4[0]}),
                create_term(PROD, {args_ZEROO_T1_T2[1], args_OType_T3_T4[1]})
            }
        );
    }

    // O : OTYPE(T1 T2) => TSR(O 0O(T3 T4)) -> 0O(PROD(T1 T3) PROD(T2 T4))
    DIRACOQ_RULE_DEF(R_TSRO1, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_O_0O_T3_T4)

        MATCH_HEAD(args_TSR_O_0O_T3_T4[1], ZEROO, args_ZEROO_T3_T4)

        // Check the typing of O
        auto type_O = kernel.calc_type(args_TSR_O_0O_T3_T4[0]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        return create_term(ZEROO, 
            {
                create_term(PROD, {args_OType_T1_T2[0], args_ZEROO_T3_T4[0]}),
                create_term(PROD, {args_OType_T1_T2[1], args_ZEROO_T3_T4[1]})
            }
        );
    }

    // TSR(1O(T1) 1O(T2)) -> 1O(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRO2, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_1O_T1_1O_T2)

        MATCH_HEAD(args_TSR_1O_T1_1O_T2[0], ONEO, args_ONEO_T1)

        MATCH_HEAD(args_TSR_1O_T1_1O_T2[1], ONEO, args_ONEO_T2)

        return create_term(ONEO, {create_term(PROD, {args_ONEO_T1[0], args_ONEO_T2[0]})});
    }

    // TSR(OUTER(K1 B1) OUTER(K2 B2)) -> OUTER(TSR(K1 K2) TSR(B1 B2))
    DIRACOQ_RULE_DEF(R_TSRO3, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_OUTER_K1_B1_OUTER_K2_B2)

        MATCH_HEAD(args_TSR_OUTER_K1_B1_OUTER_K2_B2[0], OUTER, args_OUTER_K1_B1)

        MATCH_HEAD(args_TSR_OUTER_K1_B1_OUTER_K2_B2[1], OUTER, args_OUTER_K2_B2)

        return create_term(OUTER, 
            {
                create_term(TSR, {args_OUTER_K1_B1[0], args_OUTER_K2_B2[0]}),
                create_term(TSR, {args_OUTER_K1_B1[1], args_OUTER_K2_B2[1]})
            }
        );
    }

    // MULK(0O(T1 T2) K) -> 0K(T1)
    DIRACOQ_RULE_DEF(R_MULK0, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_0O_T1_T2_K)

        MATCH_HEAD(args_MULK_0O_T1_T2_K[0], ZEROO, args_ZEROO_T1_T2)

        return create_term(ZEROK, {args_ZEROO_T1_T2[0]});
    }

    // O : OTYPE(T1 T2) => MULK(O 0K(T2)) -> 0K(T1)
    DIRACOQ_RULE_DEF(R_MULK1, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_O_0K_T2)

        if (args_MULK_O_0K_T2[1]->get_head() != ZEROK) return std::nullopt;

        // Check the typing of O
        auto type_O = kernel.calc_type(args_MULK_O_0K_T2[0]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        return create_term(ZEROK, {args_OType_T1_T2[0]});
    }

    // MULK(1O(T) K) -> K
    DIRACOQ_RULE_DEF(R_MULK2, kernel, term) {

        MATCH_HEAD(term, MULK, args_MULK_1O_T_K)

        if (args_MULK_1O_T_K[0]->get_head() != ONEO) return std::nullopt;

        return args_MULK_1O_T_K[1];
    }

    // MULK(SCR(a O) K) -> SCR(a MULK(O K))
    DIRACOQ_RULE_DEF(R_MULK3, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_SCR_a_O_K)

        MATCH_HEAD(args_MULK_SCR_a_O_K[0], SCR, args_SCR_a_O)

        return create_term(SCR, 
            {
                args_SCR_a_O[0],
                create_term(MULK, {args_SCR_a_O[1], args_MULK_SCR_a_O_K[1]})
            }
        );
    }

    // MULK(O SCR(a K)) -> SCR(a MULK(O K))
    DIRACOQ_RULE_DEF(R_MULK4, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_O_SCR_a_K)

        MATCH_HEAD(args_MULK_O_SCR_a_K[1], SCR, args_SCR_a_K)

        return create_term(SCR, 
            {
                args_SCR_a_K[0],
                create_term(MULK, {args_MULK_O_SCR_a_K[0], args_SCR_a_K[1]})
            }
        );
    }

    // MULK(ADD(O1 ... On) K) -> ADD(MULK(O1 K) ... MULK(On K))
    DIRACOQ_RULE_DEF(R_MULK5, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_ADD_O1_On_K)

        MATCH_HEAD(args_MULK_ADD_O1_On_K[0], ADD, args_ADD_O1_On)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_On) {
            new_args.push_back(create_term(MULK, {arg, args_MULK_ADD_O1_On_K[1]}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // MULK(O ADD(K1 ... Kn)) -> ADD(MULK(O K1) ... MULK(O Kn))
    DIRACOQ_RULE_DEF(R_MULK6, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_O_ADD_K1_Kn)

        MATCH_HEAD(args_MULK_O_ADD_K1_Kn[1], ADD, args_ADD_K1_Kn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_K1_Kn) {
            new_args.push_back(create_term(MULK, {args_MULK_O_ADD_K1_Kn[0], arg}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // MULK(OUTER(K1 B) K2) -> SCR(DOT(B K2) K1)
    DIRACOQ_RULE_DEF(R_MULK7, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_OUTER_K1_B_K2)

        MATCH_HEAD(args_MULK_OUTER_K1_B_K2[0], OUTER, args_OUTER_K1_B)

        return create_term(SCR, 
            {
                create_term(DOT, {args_OUTER_K1_B[1], args_MULK_OUTER_K1_B_K2[1]}),
                args_OUTER_K1_B[0]
            }
        );
    }

    // MULK(MULO(O1 O2) K) -> MULK(O1 MULK(O2 K))
    DIRACOQ_RULE_DEF(R_MULK8, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_MULO_O1_O2_K)

        MATCH_HEAD(args_MULK_MULO_O1_O2_K[0], MULO, args_MULO_O1_O2)

        return create_term(MULK, 
            {
                args_MULO_O1_O2[0],
                create_term(MULK, {args_MULO_O1_O2[1], args_MULK_MULO_O1_O2_K[1]})
            }
        );
    }

    // MULK(TSR(O1 O2) MULK(TSR(O3 O4) K)) -> MULK(TSR(MULO(O1 O3) MULO(O2 O4)) K)
    DIRACOQ_RULE_DEF(R_MULK9, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_TSR_O1_O2_MULK_TSR_O3_O4_K)

        MATCH_HEAD(args_MULK_TSR_O1_O2_MULK_TSR_O3_O4_K[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULK_TSR_O1_O2_MULK_TSR_O3_O4_K[1], MULK, args_MULK_TSR_O3_O4_K)

        MATCH_HEAD(args_MULK_TSR_O3_O4_K[0], TSR, args_TSR_O3_O4)

        return create_term(MULK, 
            {
                create_term(TSR, 
                    {
                        create_term(MULO, {args_TSR_O1_O2[0], args_TSR_O3_O4[0]}),
                        create_term(MULO, {args_TSR_O1_O2[1], args_TSR_O3_O4[1]})
                    }
                ),
                args_MULK_TSR_O3_O4_K[1]
            }
        );
    }

    // MULK(TSR(O1 O2) KET(PAIR(s t))) -> TSR(MULK(O1 KET(s)) MULK(O2 KET(t)))
    DIRACOQ_RULE_DEF(R_MULK10, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_TSR_O1_O2_KET_PAIR_s_t)

        MATCH_HEAD(args_MULK_TSR_O1_O2_KET_PAIR_s_t[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULK_TSR_O1_O2_KET_PAIR_s_t[1], KET, args_KET_PAIR_s_t)

        MATCH_HEAD(args_KET_PAIR_s_t[0], PAIR, args_PAIR_s_t)

        return create_term(TSR, 
            {
                create_term(MULK, {args_TSR_O1_O2[0], create_term(KET, {args_PAIR_s_t[0]})}),
                create_term(MULK, {args_TSR_O1_O2[1], create_term(KET, {args_PAIR_s_t[1]})})
            }
        );
    }

    // MULK(TSR(O1 O2) TSR(K1 K2)) -> TSR(MULK(O1 K1) MULK(O2 K2))
    DIRACOQ_RULE_DEF(R_MULK11, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_TSR_O1_O2_TSR_K1_K2)

        MATCH_HEAD(args_MULK_TSR_O1_O2_TSR_K1_K2[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULK_TSR_O1_O2_TSR_K1_K2[1], TSR, args_TSR_K1_K2)

        return create_term(TSR, 
            {
                create_term(MULK, {args_TSR_O1_O2[0], args_TSR_K1_K2[0]}),
                create_term(MULK, {args_TSR_O1_O2[1], args_TSR_K1_K2[1]})
            }
        );
    }

    // MULB(B 0O(T1 T2)) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_MULB0, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_B_0O_T1_T2)

        MATCH_HEAD(args_MULB_B_0O_T1_T2[1], ZEROO, args_ZEROO_T1_T2)

        return create_term(ZEROB, {args_ZEROO_T1_T2[1]});
    }

    // O : OTYPE(T1 T2) => MULB(0B(T1) O) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_MULB1, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_0B_T1_O)

        if (args_MULB_0B_T1_O[0]->get_head() != ZEROB) return std::nullopt;

        // Check the typing of O
        auto type_O = kernel.calc_type(args_MULB_0B_T1_O[1]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        return create_term(ZEROB, {args_OType_T1_T2[1]});
    }

    // MULB(B 1O(T)) -> B
    DIRACOQ_RULE_DEF(R_MULB2, kernel, term) {

        MATCH_HEAD(term, MULB, args_MULB_B_1O_T)

        if (args_MULB_B_1O_T[1]->get_head() != ONEO) return std::nullopt;

        return args_MULB_B_1O_T[0];
    }

    // MULB(SCR(a B) O) -> SCR(a MULB(B O))
    DIRACOQ_RULE_DEF(R_MULB3, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_SCR_a_B_O)

        MATCH_HEAD(args_MULB_SCR_a_B_O[0], SCR, args_SCR_a_B)

        return create_term(SCR, 
            {
                args_SCR_a_B[0],
                create_term(MULB, {args_SCR_a_B[1], args_MULB_SCR_a_B_O[1]})
            }
        );
    }

    // MULB(B SCR(a O)) -> SCR(a MULB(B O))
    DIRACOQ_RULE_DEF(R_MULB4, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_B_SCR_a_O)

        MATCH_HEAD(args_MULB_B_SCR_a_O[1], SCR, args_SCR_a_O)

        return create_term(SCR, 
            {
                args_SCR_a_O[0],
                create_term(MULB, {args_MULB_B_SCR_a_O[0], args_SCR_a_O[1]})
            }
        );
    }

    // MULB(ADD(B1 ... Bn) O) -> ADD(MULB(B1 O) ... MULB(Bn O))
    DIRACOQ_RULE_DEF(R_MULB5, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_ADD_B1_Bn_O)

        MATCH_HEAD(args_MULB_ADD_B1_Bn_O[0], ADD, args_ADD_B1_Bn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_B1_Bn) {
            new_args.push_back(create_term(MULB, {arg, args_MULB_ADD_B1_Bn_O[1]}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // MULB(B ADD(O1 ... On)) -> ADD(MULB(B O1) ... MULB(B On))
    DIRACOQ_RULE_DEF(R_MULB6, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_B_ADD_O1_On)

        MATCH_HEAD(args_MULB_B_ADD_O1_On[1], ADD, args_ADD_O1_On)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_On) {
            new_args.push_back(create_term(MULB, {args_MULB_B_ADD_O1_On[0], arg}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // MULB(B1 OUTER(K B2)) -> SCR(DOT(B1 K) B2)
    DIRACOQ_RULE_DEF(R_MULB7, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_B1_OUTER_K_B2)

        MATCH_HEAD(args_MULB_B1_OUTER_K_B2[1], OUTER, args_OUTER_K_B2)

        return create_term(SCR, 
            {
                create_term(DOT, {args_MULB_B1_OUTER_K_B2[0], args_OUTER_K_B2[0]}),
                args_OUTER_K_B2[1]
            }
        );
    }

    // MULB(B MULO(O1 O2)) -> MULB(MULB(B O1) O2)
    DIRACOQ_RULE_DEF(R_MULB8, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_B_MULO_O1_O2)

        MATCH_HEAD(args_MULB_B_MULO_O1_O2[1], MULO, args_MULO_O1_O2)

        return create_term(MULB, 
            {
                create_term(MULB, {args_MULB_B_MULO_O1_O2[0], args_MULO_O1_O2[0]}),
                args_MULO_O1_O2[1]
            }
        );
    }

    // MULB(MULB(B TSR(O1 O2)) TSR(O3 O4)) -> MULB(B TSR(MULO(O1 O3) MULO(O2 O4)))
    DIRACOQ_RULE_DEF(R_MULB9, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_MULB_B_TSR_O1_O2_TSR_O3_O4)

        MATCH_HEAD(args_MULB_MULB_B_TSR_O1_O2_TSR_O3_O4[0], MULB, args_MULB_B_TSR_O1_O2)

        MATCH_HEAD(args_MULB_B_TSR_O1_O2[1], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULB_MULB_B_TSR_O1_O2_TSR_O3_O4[1], TSR, args_TSR_O3_O4)

        return create_term(MULB, 
            {
                args_MULB_B_TSR_O1_O2[0],
                create_term(TSR, 
                    {
                        create_term(MULO, {args_TSR_O1_O2[0], args_TSR_O3_O4[0]}),
                        create_term(MULO, {args_TSR_O1_O2[1], args_TSR_O3_O4[1]})
                    }
                )
            }
        );
    }

    // MULB(BRA(PAIR(s t)) TSR(O1 O2)) -> TSR(MULB(BRA(s) O1) MULB(BRA(t) O2))
    DIRACOQ_RULE_DEF(R_MULB10, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_BRA_PAIR_s_t_TSR_O1_O2)

        MATCH_HEAD(args_MULB_BRA_PAIR_s_t_TSR_O1_O2[0], BRA, args_BRA_PAIR_s_t)

        MATCH_HEAD(args_BRA_PAIR_s_t[0], PAIR, args_PAIR_s_t)

        MATCH_HEAD(args_MULB_BRA_PAIR_s_t_TSR_O1_O2[1], TSR, args_TSR_O1_O2)

        return create_term(TSR, 
            {
                create_term(MULB, {create_term(BRA, {args_PAIR_s_t[0]}), args_TSR_O1_O2[0]}),
                create_term(MULB, {create_term(BRA, {args_PAIR_s_t[1]}), args_TSR_O1_O2[1]})
            }
        );
    }

    // MULB(TSR(B1 B2) TSR(O1 O2)) -> TSR(MULB(B1 O1) MULB(B2 O2))
    DIRACOQ_RULE_DEF(R_MULB11, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_TSR_B1_B2_TSR_O1_O2)

        MATCH_HEAD(args_MULB_TSR_B1_B2_TSR_O1_O2[0], TSR, args_TSR_B1_B2)

        MATCH_HEAD(args_MULB_TSR_B1_B2_TSR_O1_O2[1], TSR, args_TSR_O1_O2)

        return create_term(TSR, 
            {
                create_term(MULB, {args_TSR_B1_B2[0], args_TSR_O1_O2[0]}),
                create_term(MULB, {args_TSR_B1_B2[1], args_TSR_O1_O2[1]})
            }
        );
    }

    // B : B(T2) => OUTER(0K(T1) B) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_OUTER0, kernel, term) {


        MATCH_HEAD(term, OUTER, args_OUTER_0K_T1_B)

        MATCH_HEAD(args_OUTER_0K_T1_B[0], ZEROK, args_ZEROK_T1)

        // Check the typing of B
        auto type_B = kernel.calc_type(args_OUTER_0K_T1_B[1]);

        MATCH_HEAD(type_B, BTYPE, args_BType_T2)

        return create_term(ZEROO, {args_ZEROK_T1[0], args_BType_T2[0]});
    }

    // K : K(T1) => OUTER(K 0B(T2)) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_OUTER1, kernel, term) {


        MATCH_HEAD(term, OUTER, args_OUTER_K_0B_T2)

        MATCH_HEAD(args_OUTER_K_0B_T2[1], ZEROB, args_ZEROB_T2)

        // Check the typing of K
        auto type_K = kernel.calc_type(args_OUTER_K_0B_T2[0]);

        MATCH_HEAD(type_K, KTYPE, args_KType_T1)

        return create_term(ZEROO, {args_KType_T1[0], args_ZEROB_T2[0]});
    }

    // OUTER(SCR(a K) B) -> SCR(a OUTER(K B))
    DIRACOQ_RULE_DEF(R_OUTER2, kernel, term) {


        MATCH_HEAD(term, OUTER, args_OUTER_SCR_a_K_B)

        MATCH_HEAD(args_OUTER_SCR_a_K_B[0], SCR, args_SCR_a_K)

        return create_term(SCR, 
            {
                args_SCR_a_K[0],
                create_term(OUTER, {args_SCR_a_K[1], args_OUTER_SCR_a_K_B[1]})
            }
        );
    }

    // OUTER(K SCR(a B)) -> SCR(a OUTER(K B))
    DIRACOQ_RULE_DEF(R_OUTER3, kernel, term) {


        MATCH_HEAD(term, OUTER, args_OUTER_K_SCR_a_B)

        MATCH_HEAD(args_OUTER_K_SCR_a_B[1], SCR, args_SCR_a_B)

        return create_term(SCR, 
            {
                args_SCR_a_B[0],
                create_term(OUTER, {args_OUTER_K_SCR_a_B[0], args_SCR_a_B[1]})
            }
        );
    }

    // OUTER(ADD(K1 ... Kn) B) -> ADD(OUTER(K1 B) ... OUTER(Kn B))
    DIRACOQ_RULE_DEF(R_OUTER4, kernel, term) {


        MATCH_HEAD(term, OUTER, args_OUTER_ADD_K1_Kn_B)

        MATCH_HEAD(args_OUTER_ADD_K1_Kn_B[0], ADD, args_ADD_K1_Kn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_K1_Kn) {
            new_args.push_back(create_term(OUTER, {arg, args_OUTER_ADD_K1_Kn_B[1]}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // OUTER(K ADD(B1 ... Bn)) -> ADD(OUTER(K B1) ... OUTER(K Bn))
    DIRACOQ_RULE_DEF(R_OUTER5, kernel, term) {


        MATCH_HEAD(term, OUTER, args_OUTER_K_ADD_B1_Bn)

        MATCH_HEAD(args_OUTER_K_ADD_B1_Bn[1], ADD, args_ADD_B1_Bn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_B1_Bn) {
            new_args.push_back(create_term(OUTER, {args_OUTER_K_ADD_B1_Bn[0], arg}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // O : OTYPE(T2 T3) => MULO(0O(T1 T2) O) -> 0O(T1 T3)
    DIRACOQ_RULE_DEF(R_MULO0, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_0O_T1_T2_O)

        MATCH_HEAD(args_MULO_0O_T1_T2_O[0], ZEROO, args_ZEROO_T1_T2)

        // Check the typing of O
        auto type_O = kernel.calc_type(args_MULO_0O_T1_T2_O[1]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T2_T3)

        return create_term(ZEROO, {args_ZEROO_T1_T2[0], args_OType_T2_T3[1]});
    }

    // O : OTYPE(T1 T2) => MULO(O 0O(T2 T3)) -> 0O(T1 T3)
    DIRACOQ_RULE_DEF(R_MULO1, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_O_0O_T2_T3)

        MATCH_HEAD(args_MULO_O_0O_T2_T3[1], ZEROO, args_ZEROO_T2_T3)

        // Check the typing of O
        auto type_O = kernel.calc_type(args_MULO_O_0O_T2_T3[0]);

        MATCH_HEAD(type_O, OTYPE, args_OType_T1_T2)

        return create_term(ZEROO, {args_OType_T1_T2[0], args_ZEROO_T2_T3[1]});
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


        MATCH_HEAD(term, MULO, args_MULO_OUTER_K_B_O)

        MATCH_HEAD(args_MULO_OUTER_K_B_O[0], OUTER, args_OUTER_K_B)

        return create_term(OUTER, 
            {
                args_OUTER_K_B[0],
                create_term(MULB, {args_OUTER_K_B[1], args_MULO_OUTER_K_B_O[1]})
            }
        );
    }

    // MULO(O OUTER(K B)) -> OUTER(MULK(O K) B)
    DIRACOQ_RULE_DEF(R_MULO5, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_O_OUTER_K_B)

        MATCH_HEAD(args_MULO_O_OUTER_K_B[1], OUTER, args_OUTER_K_B)

        return create_term(OUTER, 
            {
                create_term(MULK, {args_MULO_O_OUTER_K_B[0], args_OUTER_K_B[0]}),
                args_OUTER_K_B[1]
            }
        );
    }

    // MULO(SCR(a O1) O2) -> SCR(a MULO(O1 O2))
    DIRACOQ_RULE_DEF(R_MULO6, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_SCR_a_O1_O2)

        MATCH_HEAD(args_MULO_SCR_a_O1_O2[0], SCR, args_SCR_a_O1)

        return create_term(SCR, 
            {
                args_SCR_a_O1[0],
                create_term(MULO, {args_SCR_a_O1[1], args_MULO_SCR_a_O1_O2[1]})
            }
        );
    }

    // MULO(O1 SCR(a O2)) -> SCR(a MULO(O1 O2))
    DIRACOQ_RULE_DEF(R_MULO7, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_O1_SCR_a_O2)

        MATCH_HEAD(args_MULO_O1_SCR_a_O2[1], SCR, args_SCR_a_O2)

        return create_term(SCR, 
            {
                args_SCR_a_O2[0],
                create_term(MULO, {args_MULO_O1_SCR_a_O2[0], args_SCR_a_O2[1]})
            }
        );
    }

    // MULO(ADD(O1 ... On) O) -> ADD(MULO(O1 O) ... MULO(On O))
    DIRACOQ_RULE_DEF(R_MULO8, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_ADD_O1_On_O)

        MATCH_HEAD(args_MULO_ADD_O1_On_O[0], ADD, args_ADD_O1_On)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_On) {
            new_args.push_back(create_term(MULO, {arg, args_MULO_ADD_O1_On_O[1]}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // MULO(O ADD(O1 ... On)) -> ADD(MULO(O O1) ... MULO(O On))
    DIRACOQ_RULE_DEF(R_MULO9, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_O_ADD_O1_On)

        MATCH_HEAD(args_MULO_O_ADD_O1_On[1], ADD, args_ADD_O1_On)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_O1_On) {
            new_args.push_back(create_term(MULO, {args_MULO_O_ADD_O1_On[0], arg}));
        }
        return create_term(ADD, std::move(new_args));
    }

    // MULO(MULO(O1 O2) O3) -> MULO(O1 MULO(O2 O3))
    DIRACOQ_RULE_DEF(R_MULO10, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_MULO_O1_O2_O3)

        MATCH_HEAD(args_MULO_MULO_O1_O2_O3[0], MULO, args_MULO_O1_O2)

        return create_term(MULO, 
            {
                args_MULO_O1_O2[0],
                create_term(MULO, {args_MULO_O1_O2[1], args_MULO_MULO_O1_O2_O3[1]})
            }
        );
    }

    // MULO(TSR(O1 O2) TSR(O3 O4)) -> TSR(MULO(O1 O3) MULO(O2 O4))
    DIRACOQ_RULE_DEF(R_MULO11, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_TSR_O1_O2_TSR_O3_O4)

        MATCH_HEAD(args_MULO_TSR_O1_O2_TSR_O3_O4[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULO_TSR_O1_O2_TSR_O3_O4[1], TSR, args_TSR_O3_O4)

        return create_term(TSR, 
            {
                create_term(MULO, {args_TSR_O1_O2[0], args_TSR_O3_O4[0]}),
                create_term(MULO, {args_TSR_O1_O2[1], args_TSR_O3_O4[1]})
            }
        );
    }

    // MULO(TSR(O1 O2) MULO(TSR(O3 O4) O)) -> MULO(TSR(MULO(O1 O3) MULO(O2 O4)) O)
    DIRACOQ_RULE_DEF(R_MULO12, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_TSR_O1_O2_MULO_TSR_O3_O4_O)

        MATCH_HEAD(args_MULO_TSR_O1_O2_MULO_TSR_O3_O4_O[0], TSR, args_TSR_O1_O2)

        MATCH_HEAD(args_MULO_TSR_O1_O2_MULO_TSR_O3_O4_O[1], MULO, args_MULO_TSR_O3_O4_O)

        MATCH_HEAD(args_MULO_TSR_O3_O4_O[0], TSR, args_TSR_O3_O4)

        return create_term(MULO, 
            {
                create_term(TSR, 
                    {
                        create_term(MULO, {args_TSR_O1_O2[0], args_TSR_O3_O4[0]}),
                        create_term(MULO, {args_TSR_O1_O2[1], args_TSR_O3_O4[1]})
                    }
                ),
                args_MULO_TSR_O3_O4_O[1]
            }
        );
    }

    // CATPROD(USET(T1) USET(T2)) -> USET(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_SET0, kernel, term) {


        MATCH_HEAD(term, CATPROD, args_CATPROD_USET_T1_USET_T2)

        MATCH_HEAD(args_CATPROD_USET_T1_USET_T2[0], USET, args_USET_T1)

        MATCH_HEAD(args_CATPROD_USET_T1_USET_T2[1], USET, args_USET_T2)

        return create_term(USET, {create_term(PROD, {args_USET_T1[0], args_USET_T2[0]})});
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
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, ONEO, args_ONEO_T)

        auto new_var_int = kernel.register_symbol(sig.unique_var());
        auto new_var = create_term(new_var_int);

        return create_term(SUM, 
            {
                create_term(USET, {args_ONEO_T[0]}),
                create_term(FUN, 
                    {
                        new_var,
                        create_term(BASIS, {args_ONEO_T[0]}),
                        create_term(OUTER, 
                            {
                                create_term(KET, {new_var}),
                                create_term(BRA, {new_var})
                            }
                        )
                    }
                )
            }
        );
    }

    // i free in t => SUM(USET(T) FUN(i T SUM(... DELTA(i t) ...))) -> SUM(... 1 ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM0, kernel, term) {


        MATCH_HEAD(term, SUM, args_SUM_USET_T_fun_i_T_SUM_DELTA_i_t)

        if (args_SUM_USET_T_fun_i_T_SUM_DELTA_i_t[0]->get_head() != USET) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_T_fun_i_T_SUM_DELTA_i_t[1], FUN, args_FUN_i_T_SUM_DELTA_i_t)

        TermPtr<int> inner_term = args_FUN_i_T_SUM_DELTA_i_t[2];

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

        TermPtr<int> i, t;
        // Check delta_{i, t} and whether i is free in t
        if (*args_DELTA_i_t[0] == *args_FUN_i_T_SUM_DELTA_i_t[0]) {
            i = args_DELTA_i_t[0];
            t = args_DELTA_i_t[1];
        }
        else if (*args_DELTA_i_t[1] == *args_FUN_i_T_SUM_DELTA_i_t[0]) {
            i = args_DELTA_i_t[1];
            t = args_DELTA_i_t[0];
        }
        else {
            return std::nullopt;
        }
            
        if (!free_in(t, i->get_head())) return std::nullopt;

        return args_FUN_i_T_SUM_DELTA_i_t[2]->replace_term(inner_term, create_term(ONE));  
    }

    // i free in t => SUM(USET(T) FUN(i T SUM(... MULS(a1 ... DELTA(i t) ... an) ...))) -> SUM(... MULS(a1{i/t} ... an{i/t}) ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM1, kernel, term) {

        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_T_fun_i_T_SUM_a1_DELTA_i_t_an)

        if (args_SUM_USET_T_fun_i_T_SUM_a1_DELTA_i_t_an[0]->get_head() != USET) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_T_fun_i_T_SUM_a1_DELTA_i_t_an[1], FUN, args_FUN_i_T_SUM_a1_DELTA_i_t_an)

        TermPtr<int> inner_term = args_FUN_i_T_SUM_a1_DELTA_i_t_an[2];

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

                TermPtr<int> i, t;
                // Check delta_{i, t} and whether i is free in t
                if (*args_DELTA_i_t[0] == *args_FUN_i_T_SUM_a1_DELTA_i_t_an[0]) {
                    i = args_DELTA_i_t[0];
                    t = args_DELTA_i_t[1];
                }
                else if (*args_DELTA_i_t[1] == *args_FUN_i_T_SUM_a1_DELTA_i_t_an[0]) {
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
                        new_mul_args.push_back(subst(sig, args_MULS_a1_DELTA_i_t_an[j], i->get_head(), t));
                    }
                }


                return args_FUN_i_T_SUM_a1_DELTA_i_t_an[2]->replace_term(inner_term, create_term(MULS, std::move(new_mul_args)));  

            }
        }

        return std::nullopt;
    }

    // i free in t => SUM(USET(T) FUN(i T SUM(... SCR(DELTA(i t) A) ...))) -> SUM(... A{i/t} ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM2, kernel, term) {

        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_T_fun_i_T_SUM_SCR_DELTA_i_t_A)

        if (args_SUM_USET_T_fun_i_T_SUM_SCR_DELTA_i_t_A[0]->get_head() != USET) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_T_fun_i_T_SUM_SCR_DELTA_i_t_A[1], FUN, args_FUN_i_T_SUM_SCR_DELTA_i_t_A)

        TermPtr<int> inner_term = args_FUN_i_T_SUM_SCR_DELTA_i_t_A[2];

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

        TermPtr<int> i, t;
        // Check delta_{i, t} and whether i is free in t
        if (*args_DELTA_i_t[0] == *args_FUN_i_T_SUM_SCR_DELTA_i_t_A[0]) {
            i = args_DELTA_i_t[0];
            t = args_DELTA_i_t[1];
        }
        else if (*args_DELTA_i_t[1] == *args_FUN_i_T_SUM_SCR_DELTA_i_t_A[0]) {
            i = args_DELTA_i_t[1];
            t = args_DELTA_i_t[0];
        }
        else {
            return std::nullopt;
        }
            
        if (!free_in(t, i->get_head())) return std::nullopt;

        return args_FUN_i_T_SUM_SCR_DELTA_i_t_A[2]->replace_term(
            inner_term, 
            subst(sig, args_SCR_DELTA_i_t_A[1], i->get_head(), t)
        );
    }

    // i free in t => SUM(USET(T) FUN(i T SUM(... SCR(MULS(a1 ... DELTA(i t) ... an) A) ...))) -> SUM(... SCR(MULS(a1{i/t} ... an{i/t}) A{i/t}) ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM3, kernel, term) {

        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_T_fun_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A)

        if (args_SUM_USET_T_fun_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[0]->get_head() != USET) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_T_fun_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[1], FUN, args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A)

        TermPtr<int> inner_term = args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[2];

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

                TermPtr<int> i, t;
                // Check delta_{i, t} and whether i is free in t
                if (*args_DELTA_i_t[0] == *args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[0]) {
                    i = args_DELTA_i_t[0];
                    t = args_DELTA_i_t[1];
                }
                else if (*args_DELTA_i_t[1] == *args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[0]) {
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
                        new_mul_args.push_back(subst(sig, args_MULS_a1_DELTA_i_t_an[j], i->get_head(), t));
                    }
                }

                return args_FUN_i_T_SUM_SCR_MULS_a1_DELTA_i_t_an_A[2]->replace_term(
                    inner_term, 
                    create_term(SCR, 
                        {
                            create_term(MULS, std::move(new_mul_args)), 
                            subst(sig, args_SCR_MULS_a1_DELTA_i_t_an_A[1], i->get_head(), t)
                        }
                    )
                );
            }
        }

        return std::nullopt;
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... DELTA(i j) ...))))) -> SUM(M FUN(j T SUM(... 1 ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM4, kernel, term) {

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j)

        MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j)

        MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[2], SUM, args_SUM_M_fun_j_T_SUM_DELTA_i_j)

        // Check that the summation set is the same

        if (*args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[0] != *args_SUM_M_fun_j_T_SUM_DELTA_i_j[0]) return std::nullopt;

        MATCH_HEAD(args_SUM_M_fun_j_T_SUM_DELTA_i_j[1], FUN, args_FUN_j_T_SUM_DELTA_i_j)

        TermPtr<int> inner_term = args_FUN_j_T_SUM_DELTA_i_j[2];

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
        if (!((*args_DELTA_i_j[0] == *args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[0] && *args_DELTA_i_j[1] == *args_FUN_j_T_SUM_DELTA_i_j[0]) || 
            (*args_DELTA_i_j[1] == *args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[0] && *args_DELTA_i_j[0] == *args_FUN_j_T_SUM_DELTA_i_j[0]))) return std::nullopt;

        return args_FUN_i_T_SUM_M_fun_j_T_SUM_DELTA_i_j[2]->replace_term(inner_term, create_term(ONE));
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... MULS(a1 ... DELTA(i j) ... an) ...))))) -> SUM(M FUN(j T SUM(... MULS(a1{j/i} ... an{j/i}) ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM5, kernel, term) {

        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an)

        MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an)

        MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[2], SUM, args_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an)

        // Check that the summation set is the same

        if (*args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0] != *args_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0]) return std::nullopt;

        MATCH_HEAD(args_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[1], FUN, args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an)

        TermPtr<int> inner_term = args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an[2];

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
                if (!((*args_DELTA_i_j[0] == *args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0] && *args_DELTA_i_j[1] == *args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an[0]) || 
                    (*args_DELTA_i_j[1] == *args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0] && *args_DELTA_i_j[0] == *args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an[0]))) continue;

                ListArgs<int> new_mul_args;
                for (int j = 0; j < args_MULS_a1_DELTA_i_j_an.size(); j++) {
                    if (j == idx_i) {
                        continue;
                    }
                    else {
                        new_mul_args.push_back(
                            subst(sig,
                                args_MULS_a1_DELTA_i_j_an[j],
                                args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[0]->get_head(),
                                args_FUN_j_T_SUM_MULS_a1_DELTA_i_j_an[0])
                        );
                    }
                }

                return args_FUN_i_T_SUM_M_fun_j_T_SUM_MULS_a1_DELTA_i_j_an[2]->replace_term(
                    inner_term, 
                    create_term(MULS, std::move(new_mul_args))
                );
            }
        }

        return std::nullopt;
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... SCR(DELTA(i j) A) ...))))) -> SUM(M FUN(j T SUM(... A{j/i} ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM6, kernel, term) {

        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A)

        MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A)

        MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[2], SUM, args_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A)

        // Check that the summation set is the same

        if (*args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0] != *args_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0]) return std::nullopt;

        MATCH_HEAD(args_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[1], FUN, args_FUN_j_T_SUM_DELTA_i_j_A)

        TermPtr<int> inner_term = args_FUN_j_T_SUM_DELTA_i_j_A[2];

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
        if (!((*args_DELTA_i_j[0] == *args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0] && *args_DELTA_i_j[1] == *args_FUN_j_T_SUM_DELTA_i_j_A[0]) || 
            (*args_DELTA_i_j[1] == *args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0] && *args_DELTA_i_j[0] == *args_FUN_j_T_SUM_DELTA_i_j_A[0]))) return std::nullopt;

        return args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[2]->replace_term(
            inner_term, 
            subst(sig, args_SCR_DELTA_i_j_A[1], args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_DELTA_i_j_A[0]->get_head(), args_FUN_j_T_SUM_DELTA_i_j_A[0])
        );
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... SCR(MULS(a1 ... DELTA(i j) ... an) A) ...))))) -> SUM(M FUN(j T SUM(... SCR(MULS(a1{j/i} ... an{j/i}) A{j/i}) ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM7, kernel, term) {

        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A)

        MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A)

        MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[2], SUM, args_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A)

        // Check that the summation set is the same

        if (*args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0] != *args_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]) return std::nullopt;

        MATCH_HEAD(args_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[1], FUN, args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A)

        TermPtr<int> inner_term = args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[2];

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
                if (!((*args_DELTA_i_j[0] == *args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0] && *args_DELTA_i_j[1] == *args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]) || 
                    (*args_DELTA_i_j[1] == *args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0] && *args_DELTA_i_j[0] == *args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]))) continue;

                ListArgs<int> new_mul_args;
                for (int j = 0; j < args_MULS_a1_DELTA_i_j_an.size(); j++) {
                    if (j == idx_i) {
                        continue;
                    }
                    else {
                        new_mul_args.push_back(
                            subst(sig,
                                args_MULS_a1_DELTA_i_j_an[j],
                                args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]->get_head(),
                                args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0])
                        );
                    }
                }

                return args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[2]->replace_term(
                    inner_term, 
                    create_term(SCR, 
                        {
                            create_term(MULS, std::move(new_mul_args)), 
                            subst(
                                sig, 
                                args_SCR_MULS_a1_DELTA_i_j_an_A[1], 
                                args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]->get_head(), 
                                args_FUN_j_T_SUM_SCR_MULS_a1_DELTA_i_j_an_A[0]
                            )
                        }
                    )
                );
            }
        }

        return std::nullopt;
    }

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... SCR(ADDS(MULS(a1 ... DELTA(i j) ... an) ... MULS(b1 ... DELTA(i j) ... bn)) A) ...))))) -> SUM(M FUN(j T SUM(... SCR(ADDS(MULS(a1{j/i} ... an{j/i}) ... MULS(b1{j/i} ... bn{j/i})) A{j/i}) ...)))
    // DIRACOQ_RULE_DEF(R_SUM_ELIM8, kernel, term) {
        
    //     auto &sig = kernel.get_sig();

    //     MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A)

    //     MATCH_HEAD(args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A[1], FUN, args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A)

    //     MATCH_HEAD(args_FUN_i_T_SUM_M_fun_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A[2], SUM, args_SUM_M_fun_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A)

    //     // Check that the summation set is the same

    //     if (*args_SUM_M_fun_i_T_SUM_M_fun_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A[0] != *args_SUM_M_fun_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A[0]) return std::nullopt;

    //     MATCH_HEAD(args_SUM_M_fun_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A[1], FUN, args_FUN_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A)

    //     TermPtr<int> inner_term = args_FUN_j_T_SUM_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A[2];

    //     while (true) {
    //         if (inner_term->get_head() == SCR) break;
    //         // continually match the function and sum inside
            
    //         if (inner_term->get_head() == SUM) {
    //             auto& args = inner_term->get_args();
            
    //             MATCH_HEAD(args[1], FUN, ars_fun_inside)
    //             inner_term = ars_fun_inside[2];
    //         }
    //         else {
    //             return std::nullopt;
    //         }
    //     }

    //     MATCH_HEAD(inner_term, SCR, args_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A)

    //     MATCH_HEAD(args_SCR_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn_A[0], ADDS, args_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn)

    //     if (args_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn.size() == 1) return std::nullopt;

    //     auto 

    //     for (int idx_i = 0; idx_i < args_ADDS_MULS_a1_DELTA_i_j_an_MULS_b1_DELTA_i_j_bn[0]->get_args().size()
    // }

    // MULS(b1 ... SUM(M FUN(i T a)) ... bn) -> SUM(M FUN(i T MULS(b1 ... a ... bn)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH0, kernel, term) {


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

            return create_term(SUM, 
                {
                    args_SUM_M_fun_i_T_a[0],
                    create_term(FUN, 
                        {
                            args_fun_i_T_a[0],
                            args_fun_i_T_a[1],
                            create_term(MULS, std::move(new_mul_args))
                        }
                    )
                }
            );
        }

        return std::nullopt;          
    }

    // CONJ(SUM(M FUN(i T a))) -> SUM(M FUN(i T CONJ(a)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH1, kernel, term) {


        MATCH_HEAD(term, CONJ, args_CONJ_SUM_M_fun_i_T_a)

        MATCH_HEAD(args_CONJ_SUM_M_fun_i_T_a[0], SUM, args_SUM_M_fun_i_T_a)

        MATCH_HEAD(args_SUM_M_fun_i_T_a[1], FUN, args_FUN_i_T_a)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_a[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_a[0],
                        args_FUN_i_T_a[1],
                        create_term(CONJ, {args_FUN_i_T_a[2]})
                    }
                )
            }
        );
    }

    // ADJ(SUM(M FUN(i T X))) -> SUM(M FUN(i T ADJ(X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH2, kernel, term) {


        MATCH_HEAD(term, ADJ, args_ADJ_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_ADJ_SUM_M_fun_i_T_X[0], SUM, args_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SUM_M_fun_i_T_X[1], FUN, args_FUN_i_T_X)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_X[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_X[0],
                        args_FUN_i_T_X[1],
                        create_term(ADJ, {args_FUN_i_T_X[2]})
                    }
                )
            }
        );
    }

    // SCR(a SUM(M FUN(i T X))) -> SUM(M FUN(i T SCR(a X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH3, kernel, term) {


        MATCH_HEAD(term, SCR, args_SCR_a_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SCR_a_SUM_M_fun_i_T_X[1], SUM, args_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SUM_M_fun_i_T_X[1], FUN, args_FUN_i_T_X)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_X[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_X[0],
                        args_FUN_i_T_X[1],
                        create_term(SCR, {args_SCR_a_SUM_M_fun_i_T_X[0], args_FUN_i_T_X[2]})
                    }
                )
            }
        );
    }

    // SCR(SUM(M FUN(i T a)) X) -> SUM(M FUN(i T SCR(a X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH4, kernel, term) {


        MATCH_HEAD(term, SCR, args_SCR_SUM_M_fun_i_T_a_X)

        MATCH_HEAD(args_SCR_SUM_M_fun_i_T_a_X[0], SUM, args_SUM_M_fun_i_T_a)

        MATCH_HEAD(args_SUM_M_fun_i_T_a[1], FUN, args_FUN_i_T_a)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_a[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_a[0],
                        args_FUN_i_T_a[1],
                        create_term(SCR, {args_FUN_i_T_a[2], args_SCR_SUM_M_fun_i_T_a_X[1]})
                    }
                )
            }
        );
    }


    // DOT(SUM(M FUN(i T B)) K) -> SUM(M FUN(i T DOT(B K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH5, kernel, term) {


        MATCH_HEAD(term, DOT, args_DOT_SUM_M_fun_i_T_B_K)

        MATCH_HEAD(args_DOT_SUM_M_fun_i_T_B_K[0], SUM, args_SUM_M_fun_i_T_B)

        MATCH_HEAD(args_SUM_M_fun_i_T_B[1], FUN, args_FUN_i_T_B)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_B[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_B[0],
                        args_FUN_i_T_B[1],
                        create_term(DOT, {args_FUN_i_T_B[2], args_DOT_SUM_M_fun_i_T_B_K[1]})
                    }
                )
            }
        );
    }

    // MULK(SUM(M FUN(i T O)) K) -> SUM(M FUN(i T MULK(O K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH6, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_SUM_M_fun_i_T_O_K)

        MATCH_HEAD(args_MULK_SUM_M_fun_i_T_O_K[0], SUM, args_SUM_M_fun_i_T_O)

        MATCH_HEAD(args_SUM_M_fun_i_T_O[1], FUN, args_FUN_i_T_O)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_O[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_O[0],
                        args_FUN_i_T_O[1],
                        create_term(MULK, {args_FUN_i_T_O[2], args_MULK_SUM_M_fun_i_T_O_K[1]})
                    }
                )
            }
        );
    }

    // MULB(SUM(M FUN(i T B)) O) -> SUM(M FUN(i T MULB(B O)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH7, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_SUM_M_fun_i_T_B_O)

        MATCH_HEAD(args_MULB_SUM_M_fun_i_T_B_O[0], SUM, args_SUM_M_fun_i_T_B)

        MATCH_HEAD(args_SUM_M_fun_i_T_B[1], FUN, args_FUN_i_T_B)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_B[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_B[0],
                        args_FUN_i_T_B[1],
                        create_term(MULB, {args_FUN_i_T_B[2], args_MULB_SUM_M_fun_i_T_B_O[1]})
                    }
                )
            }
        );
    }
    
    // OUTER(SUM(M FUN(i T K)) B) -> SUM(M FUN(i T OUTER(K B)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH8, kernel, term) {


        MATCH_HEAD(term, OUTER, args_OUTER_SUM_M_fun_i_T_K_B)

        MATCH_HEAD(args_OUTER_SUM_M_fun_i_T_K_B[0], SUM, args_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_SUM_M_fun_i_T_K[1], FUN, args_FUN_i_T_K)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_K[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_K[0],
                        args_FUN_i_T_K[1],
                        create_term(OUTER, {args_FUN_i_T_K[2], args_OUTER_SUM_M_fun_i_T_K_B[1]})
                    }
                )
            }
        );
    }
    
    // MULO(SUM(M FUN(i T O1)) O2) -> SUM(M FUN(i T MULO(O1 O2)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH9, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_SUM_M_fun_i_T_O1_O2)

        MATCH_HEAD(args_MULO_SUM_M_fun_i_T_O1_O2[0], SUM, args_SUM_M_fun_i_T_O1)

        MATCH_HEAD(args_SUM_M_fun_i_T_O1[1], FUN, args_FUN_i_T_O1)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_O1[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_O1[0],
                        args_FUN_i_T_O1[1],
                        create_term(MULO, {args_FUN_i_T_O1[2], args_MULO_SUM_M_fun_i_T_O1_O2[1]})
                    }
                )
            }
        );
    }


    // DOT(B SUM(M FUN(i T K))) -> SUM(M FUN(i T DOT(B K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH10, kernel, term) {


        MATCH_HEAD(term, DOT, args_DOT_B_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_DOT_B_SUM_M_fun_i_T_K[1], SUM, args_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_SUM_M_fun_i_T_K[1], FUN, args_FUN_i_T_K)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_K[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_K[0],
                        args_FUN_i_T_K[1],
                        create_term(DOT, {args_DOT_B_SUM_M_fun_i_T_K[0], args_FUN_i_T_K[2]})
                    }
                )
            }
        );
    }

    // MULK(O SUM(M FUN(i T K))) -> SUM(M FUN(i T MULK(O K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH11, kernel, term) {


        MATCH_HEAD(term, MULK, args_MULK_O_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_MULK_O_SUM_M_fun_i_T_K[1], SUM, args_SUM_M_fun_i_T_K)

        MATCH_HEAD(args_SUM_M_fun_i_T_K[1], FUN, args_FUN_i_T_K)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_K[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_K[0],
                        args_FUN_i_T_K[1],
                        create_term(MULK, {args_MULK_O_SUM_M_fun_i_T_K[0], args_FUN_i_T_K[2]})
                    }
                )
            }
        );
    }

    // MULB(B SUM(M FUN(i T O))) -> SUM(M FUN(i T MULB(B O)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH12, kernel, term) {


        MATCH_HEAD(term, MULB, args_MULB_B_SUM_M_fun_i_T_O)

        MATCH_HEAD(args_MULB_B_SUM_M_fun_i_T_O[1], SUM, args_SUM_M_fun_i_T_O)

        MATCH_HEAD(args_SUM_M_fun_i_T_O[1], FUN, args_FUN_i_T_O)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_O[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_O[0],
                        args_FUN_i_T_O[1],
                        create_term(MULB, {args_MULB_B_SUM_M_fun_i_T_O[0], args_FUN_i_T_O[2]})
                    }
                )
            }
        );
    }

    // OUTER(K SUM(M FUN(i T B))) -> SUM(M FUN(i T OUTER(K B)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH13, kernel, term) {


        MATCH_HEAD(term, OUTER, args_OUTER_K_SUM_M_fun_i_T_B)

        MATCH_HEAD(args_OUTER_K_SUM_M_fun_i_T_B[1], SUM, args_SUM_M_fun_i_T_B)

        MATCH_HEAD(args_SUM_M_fun_i_T_B[1], FUN, args_FUN_i_T_B)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_B[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_B[0],
                        args_FUN_i_T_B[1],
                        create_term(OUTER, {args_OUTER_K_SUM_M_fun_i_T_B[0], args_FUN_i_T_B[2]})
                    }
                )
            }
        );
    }

    // MULO(O1 SUM(M FUN(i T O2)) -> SUM(M FUN(i T MULO(O1 O2)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH14, kernel, term) {


        MATCH_HEAD(term, MULO, args_MULO_O1_SUM_M_fun_i_T_O2)

        MATCH_HEAD(args_MULO_O1_SUM_M_fun_i_T_O2[1], SUM, args_SUM_M_fun_i_T_O2)

        MATCH_HEAD(args_SUM_M_fun_i_T_O2[1], FUN, args_FUN_i_T_O2)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_O2[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_O2[0],
                        args_FUN_i_T_O2[1],
                        create_term(MULO, {args_MULO_O1_SUM_M_fun_i_T_O2[0], args_FUN_i_T_O2[2]})
                    }
                )
            }
        );
    }


    // TSR(SUM(M FUN(i T X)) Y) -> SUM(M FUN(i T TSR(X Y)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH15, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_SUM_M_fun_i_T_X_Y)

        MATCH_HEAD(args_TSR_SUM_M_fun_i_T_X_Y[0], SUM, args_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SUM_M_fun_i_T_X[1], FUN, args_FUN_i_T_X)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_X[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_X[0],
                        args_FUN_i_T_X[1],
                        create_term(TSR, {args_FUN_i_T_X[2], args_TSR_SUM_M_fun_i_T_X_Y[1]})
                    }
                )
            }
        );
    }

    // TSR(X SUM(M FUN(i T Y))) -> SUM(M FUN(i T TSR(X Y)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH16, kernel, term) {


        MATCH_HEAD(term, TSR, args_TSR_X_SUM_M_fun_i_T_Y)

        MATCH_HEAD(args_TSR_X_SUM_M_fun_i_T_Y[1], SUM, args_SUM_M_fun_i_T_Y)

        MATCH_HEAD(args_SUM_M_fun_i_T_Y[1], FUN, args_FUN_i_T_Y)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_Y[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_Y[0],
                        args_FUN_i_T_Y[1],
                        create_term(TSR, {args_TSR_X_SUM_M_fun_i_T_Y[0], args_FUN_i_T_Y[2]})
                    }
                )
            }
        );
    }


    // SUM(M FUN(i T ADDS(a1 ... an))) -> ADDS(SUM(M FUN(i T a1)) ... SUM(M FUN(i T an)))
    DIRACOQ_RULE_DEF(R_SUM_ADDS0, kernel, term) {
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_ADDS_a1_an)

        MATCH_HEAD(args_SUM_M_fun_i_T_ADDS_a1_an[1], FUN, args_FUN_i_T_ADDS_a1_an)

        MATCH_HEAD(args_FUN_i_T_ADDS_a1_an[2], ADDS, args_ADDS_a1_an)

        ListArgs<int> new_sum_args;
        for (const auto &arg : args_ADDS_a1_an) {
            auto new_var = create_term(kernel.register_symbol(sig.unique_var()));
            new_sum_args.push_back(create_term(SUM, 
                {
                    args_SUM_M_fun_i_T_ADDS_a1_an[0],
                    create_term(FUN, 
                        {
                            new_var,
                            args_FUN_i_T_ADDS_a1_an[1],
                            subst(sig, arg, args_FUN_i_T_ADDS_a1_an[0]->get_head(), new_var)
                        }
                    )
                }
            ));
        }

        return create_term(ADDS, std::move(new_sum_args));
    }


    // SUM(M FUN(i T MULS(b1 ... ADDS(a1 ... an) .. bm))) -> ADDS(SUM(M FUN(i T MULS(b1 ... a1 ... bm))) ... SUM(M FUN(i T MULS(b1 ... an ... bm))))
    DIRACOQ_RULE_DEF(R_SUM_ADDS1, kernel, term) {
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_MULS_b1_ADDS_a1_an_bm)

        MATCH_HEAD(args_SUM_M_fun_i_T_MULS_b1_ADDS_a1_an_bm[1], FUN, args_FUN_i_T_MULS_b1_ADDS_a1_an_bm)

        MATCH_HEAD(args_FUN_i_T_MULS_b1_ADDS_a1_an_bm[2], MULS, args_MULS_b1_ADDS_a1_an_bm)

        if (args_MULS_b1_ADDS_a1_an_bm.size() == 1) return std::nullopt;

        for (auto i = 0; i != args_MULS_b1_ADDS_a1_an_bm.size(); ++i) {
            if (args_MULS_b1_ADDS_a1_an_bm[i]->get_head() == ADDS) {
                auto& args_ADDS_a1_an = args_MULS_b1_ADDS_a1_an_bm[i]->get_args();

                ListArgs<int> new_sum_args;
                for (const auto &arg : args_ADDS_a1_an) {
                    auto new_var = create_term(kernel.register_symbol(sig.unique_var()));
                    ListArgs<int> new_mul_args{args_MULS_b1_ADDS_a1_an_bm};
                    new_mul_args[i] = arg;
                    new_sum_args.push_back(create_term(SUM, 
                        {
                            args_SUM_M_fun_i_T_MULS_b1_ADDS_a1_an_bm[0],
                            create_term(FUN, 
                                {
                                    new_var,
                                    args_FUN_i_T_MULS_b1_ADDS_a1_an_bm[1],
                                    subst(sig, create_term(MULS, std::move(new_mul_args)), args_FUN_i_T_MULS_b1_ADDS_a1_an_bm[0]->get_head(), new_var)
                                }
                            )
                        }
                    ));
                }

                return create_term(ADDS, std::move(new_sum_args));
            }
        }

        return std::nullopt;
    }


    // SUM(M FUN(i T ADD(a1 ... an))) -> ADD(SUM(M FUN(i T a1)) ... SUM(M FUN(i T an)))
    DIRACOQ_RULE_DEF(R_SUM_ADD0, kernel, term) {
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_ADD_a1_an)

        MATCH_HEAD(args_SUM_M_fun_i_T_ADD_a1_an[1], FUN, args_FUN_i_T_ADD_a1_an)

        MATCH_HEAD(args_FUN_i_T_ADD_a1_an[2], ADD, args_ADD_a1_an)

        ListArgs<int> new_sum_args;
        for (const auto &arg : args_ADD_a1_an) {
            auto new_var = create_term(kernel.register_symbol(sig.unique_var()));
            new_sum_args.push_back(create_term(SUM, 
                {
                    args_SUM_M_fun_i_T_ADD_a1_an[0],
                    create_term(FUN, 
                        {
                            new_var,
                            args_FUN_i_T_ADD_a1_an[1],
                            subst(sig, arg, args_FUN_i_T_ADD_a1_an[0]->get_head(), new_var)
                        }
                    )
                }
            ));
        }

        return create_term(ADD, std::move(new_sum_args));
    }



    // SUM(M FUN(i T SCR(ADDS(a1 ... an) X))) -> ADD(SUM(M FUN(i T SCR(a1 X))) ... SUM(M FUN(i T SCR(an X)))
    DIRACOQ_RULE_DEF(R_SUM_ADD1, kernel, term) {
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_M_fun_i_T_SCR_ADDS_a1_an_X)

        MATCH_HEAD(args_SUM_M_fun_i_T_SCR_ADDS_a1_an_X[1], FUN, args_FUN_i_T_SCR_ADDS_a1_an_X)

        MATCH_HEAD(args_FUN_i_T_SCR_ADDS_a1_an_X[2], SCR, args_SCR_ADDS_a1_an_X)

        MATCH_HEAD(args_SCR_ADDS_a1_an_X[0], ADDS, args_ADDS_a1_an_X)

        ListArgs<int> new_sum_args;

        for (const auto &arg : args_ADDS_a1_an_X) {
            auto new_var = create_term(kernel.register_symbol(sig.unique_var()));
            new_sum_args.push_back(create_term(SUM, 
                {
                    args_SUM_M_fun_i_T_SCR_ADDS_a1_an_X[0],
                    create_term(FUN, 
                        {
                            new_var,
                            args_FUN_i_T_SCR_ADDS_a1_an_X[1],
                            subst(sig, create_term(SCR, {arg, args_SCR_ADDS_a1_an_X[1]}), args_FUN_i_T_SCR_ADDS_a1_an_X[0]->get_head(), new_var)
                        }
                    )
                }
            ));
        }

        return create_term(ADD, std::move(new_sum_args));
    }

    // SUM(USET(PROD(T1 T2)) FUN(i BASIS(PROD(T1 T2)) X)) -> SUM(USET(T1) FUN(j BASIS(T1) SUM(USET(T2) FUN(k BASIS(T2) X{i/PAIR(j k)}))))
    DIRACOQ_RULE_DEF(R_SUM_INDEX0, kernel, term) {
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_Prod_T1_T2_fun_i_Prod_T1_T2_X)

        MATCH_HEAD(args_SUM_USET_Prod_T1_T2_fun_i_Prod_T1_T2_X[0], USET, args_USET_Prod_T1_T2)

        MATCH_HEAD(args_USET_Prod_T1_T2[0], PROD, args_Prod_T1_T2)

        MATCH_HEAD(args_SUM_USET_Prod_T1_T2_fun_i_Prod_T1_T2_X[1], FUN, args_FUN_i_Prod_T1_T2_X)

        MATCH_HEAD(args_FUN_i_Prod_T1_T2_X[1], BASIS, args_Basis_Prod_T1_T2)

        if (args_Basis_Prod_T1_T2[0]->get_head() != PROD) return std::nullopt;

        TermPtr<int> j = create_term(kernel.register_symbol(sig.unique_var()));
        TermPtr<int> k = create_term(kernel.register_symbol(sig.unique_var()));

        return create_term(SUM, 
            {
                create_term(USET, {args_Prod_T1_T2[0]}),
                create_term(FUN, 
                    {
                        j,
                        create_term(BASIS, {args_Prod_T1_T2[0]}),
                        create_term(SUM, 
                            {
                                create_term(USET, {args_Prod_T1_T2[1]}),
                                create_term(FUN, 
                                    {
                                        k,
                                        create_term(BASIS, {args_Prod_T1_T2[1]}),
                                        subst(sig, args_FUN_i_Prod_T1_T2_X[2], args_FUN_i_Prod_T1_T2_X[0]->get_head(), create_term(PAIR, {j, k}))
                                    }
                                )
                            }
                        )
                    }
                )
            }
        );              
    }



    // SUM(CATPROD(M1 M2) FUN(i BASIS(PROD(T1 T2)) X)) -> SUM(M1 FUN(j BASIS(T1) SUM(M2 FUN(k BASIS(T2) X{i/PAIR(j k)})))
    DIRACOQ_RULE_DEF(R_SUM_INDEX1, kernel, term) {
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_CATPROD_M1_M2_fun_i_Prod_T1_T2_X)

        MATCH_HEAD(args_SUM_CATPROD_M1_M2_fun_i_Prod_T1_T2_X[0], CATPROD, args_CATPROD_M1_M2)

        MATCH_HEAD(args_SUM_CATPROD_M1_M2_fun_i_Prod_T1_T2_X[1], FUN, args_FUN_i_Basis_Prod_T1_T2_X)

        MATCH_HEAD(args_FUN_i_Basis_Prod_T1_T2_X[1], BASIS, args_BASIS_Prod_T1_T2)

        MATCH_HEAD(args_BASIS_Prod_T1_T2[0], PROD, args_Prod_T1_T2)

        TermPtr<int> j = create_term(kernel.register_symbol(sig.unique_var()));
        TermPtr<int> k = create_term(kernel.register_symbol(sig.unique_var()));

        return create_term(SUM, 
            {
                args_CATPROD_M1_M2[0], 
                create_term(FUN, 
                    {
                        j,
                        create_term(BASIS, {args_Prod_T1_T2[0]}),
                        create_term(SUM, 
                            {
                                args_CATPROD_M1_M2[1],
                                create_term(FUN, 
                                    {
                                        k,
                                        create_term(BASIS, {args_Prod_T1_T2[1]}),
                                        subst(sig, args_FUN_i_Basis_Prod_T1_T2_X[2], args_FUN_i_Basis_Prod_T1_T2_X[0]->get_head(), create_term(PAIR, {j, k}))
                                    }
                                )
                            }
                        )
                    }
                )
            }
        );
    }

    // return the factorized result
    optional<TermPtr<int>> sum_to_factorize(Kernel& kernel, TermPtr<int> termA, TermPtr<int> termB, vector<pair<int, int>>& bound_var_corr) {

        auto headA = termA->get_head();
        auto headB = termB->get_head();
        auto argsA = termA->get_args();
        auto argsB = termB->get_args();

        if (headA == SUM && headB == SUM && argsA[1]->get_head() == FUN && argsB[1]->get_head() == FUN) {
            auto& argsA_body = argsA[1]->get_args();
            auto& argsB_body = argsB[1]->get_args();

            // The types should be equivalent
            if (*argsA_body[1] == *argsB_body[1]) {    
                auto bound_varA = argsA_body[0];
                auto bound_varB = argsB_body[0];
                
                bound_var_corr.push_back({bound_varA->get_head(), bound_varB->get_head()});
                auto inner_res = sum_to_factorize(kernel, argsA_body[2], argsB_body[2], bound_var_corr);
                bound_var_corr.pop_back();

                if (!inner_res) return std::nullopt;

                return create_term(SUM, {argsA[0], create_term(FUN, {bound_varA, argsA_body[1], inner_res.value()})});
            }
        }

        // replace the bound variables on termB
        auto new_termB = termB;
        for (const auto& [varA, varB] : bound_var_corr) {
            new_termB = subst(kernel.get_sig(), new_termB, varB, create_term(varA));
        }

        headB = new_termB->get_head();
        argsB = new_termB->get_args();

        // A + A -> (1 + 1).A
        if (*termA == *new_termB) {
            return create_term(SCR, {create_term(ADDS, {create_term(ONE), create_term(ONE)}), termA});
        }

        // a.A + A -> (a + 1).A
        else if (headA == SCR && *argsA[1] == *new_termB) {
            return create_term(SCR, {create_term(ADDS, {argsA[0], create_term(ONE)}), new_termB});
        }

        // A + a.A -> (1 + a).A
        else if (headB == SCR && *termA == *argsB[1]) {
            return create_term(SCR, {create_term(ADDS, {create_term(ONE), argsB[0]}), termA});
        }

        // a.A + b.A -> (a + b).A
        else if (headA == SCR && headB == SCR && *argsA[1] == *argsB[1]) {
            return create_term(SCR, {create_term(ADDS, {argsA[0], argsB[0]}), argsA[1]});
        }
        else {
            return std::nullopt;
        }
    }

    DIRACOQ_RULE_DEF(R_SUM_FACTOR, kernel, term) {
        auto head = term->get_head();
        auto args = term->get_args();
        if (head != ADD && head != ADDS) return std::nullopt;

        // Find the two terms that can be factorized
        for (int i = 0; i < args.size(); i++) {
            for (int j = i + 1; j < args.size(); j++) {
                vector<pair<int, int>> bound_var_corr;
                auto factorized = sum_to_factorize(kernel, args[i], args[j], bound_var_corr);
                if (factorized) {
                    ListArgs<int> new_args;
                    for (int k = 0; k < args.size(); k++) {
                        if (k == i || k == j) continue;
                        new_args.push_back(args[k]);
                    }
                    new_args.push_back(factorized.value());
                    return create_term(head, std::move(new_args));
                }
            }
        }

        return std::nullopt;
    }


    // DELTA(BASIS0 BASIS1) -> 0
    DIRACOQ_RULE_DEF(R_QBIT_DELTA, kernel, term) {

        MATCH_HEAD(term, DELTA, args_DELTA_BASIS0_BASIS1)

        auto head_0 = args_DELTA_BASIS0_BASIS1[0]->get_head();
        auto head_1 = args_DELTA_BASIS0_BASIS1[1]->get_head();

        if (!((head_0 == BASIS0 && head_1 == BASIS1) || (head_0 == BASIS1 && head_1 == BASIS0))) return std::nullopt;

        return create_term(ZERO);
    }

    // ONEO(QBIT) -> ADD(OUTER(KET(#0) BRA(#0)) OUTER(KET(#1) BRA(#1))
    DIRACOQ_RULE_DEF(R_QBIT_ONEO, kernel, term) {

        MATCH_HEAD(term, ONEO, args_ONEO_QBIT)

        if (args_ONEO_QBIT[0]->get_head() != QBIT) return std::nullopt;

        return create_term(ADD, 
            {
                create_term(OUTER, {create_term(KET, {create_term(BASIS0)}), create_term(BRA, {create_term(BASIS0)})}),
                create_term(OUTER, {create_term(KET, {create_term(BASIS1)}), create_term(BRA, {create_term(BASIS1)})})
            }
        );
    }

    // SUM(USET(QBIT) FUN(i BASIS(QBIT) X)) -> ADD(X{i/#0} X{i/#1})
    DIRACOQ_RULE_DEF(R_QBIT_SUM, kernel, term) {
        auto &sig = kernel.get_sig();

        MATCH_HEAD(term, SUM, args_SUM_USET_QBIT_FUN_i_BASIS_QBIT_X)

        MATCH_HEAD(args_SUM_USET_QBIT_FUN_i_BASIS_QBIT_X[0], USET, args_USET_QBIT)

        if (args_USET_QBIT[0]->get_head() != QBIT) return std::nullopt;

        MATCH_HEAD(args_SUM_USET_QBIT_FUN_i_BASIS_QBIT_X[1], FUN, args_FUN_i_BASIS_QBIT_X)

        return create_term(ADD, 
            {
                subst(sig, args_FUN_i_BASIS_QBIT_X[2], args_FUN_i_BASIS_QBIT_X[0]->get_head(), create_term(BASIS0)),
                subst(sig, args_FUN_i_BASIS_QBIT_X[2], args_FUN_i_BASIS_QBIT_X[0]->get_head(), create_term(BASIS1))
            }
        );
    }

    struct L_expand_element {
        int bound_var;
        TermPtr<int> index;
        int reg;
    };

    pair<TermPtr<int>, vector<L_expand_element>> get_L_expand_info(Kernel& kernel, TermPtr<int> reg) {
        if (reg->is_atomic()) {
            auto type = kernel.calc_type(reg);
            if (type->get_head() != REG) throw std::runtime_error("get_L_expand_info: not a register");

            auto new_bound = kernel.register_symbol(kernel.get_sig().unique_var());
            return {create_term(new_bound), vector{L_expand_element{new_bound, type->get_args()[0], reg->get_head()}}};
        }

        else if (reg->get_head() == PAIR) {
            auto [left, left_info] = get_L_expand_info(kernel, reg->get_args()[0]);
            auto [right, right_info] = get_L_expand_info(kernel, reg->get_args()[1]);
            left_info.insert(left_info.end(), right_info.begin(), right_info.end());
            return {create_term(PAIR, {left, right}), left_info};
        }

        else {
            throw std::runtime_error("get_L_expand_info: invalid term");
        }
    }

    DIRACOQ_RULE_DEF(R_LABEL_EXPAND, kernel, term) {
        MATCH_HEAD(term, SUBS, args_SUBS_term_reg)

        auto type = kernel.calc_type(term);

        if (type->get_head() != DTYPE) return std::nullopt;

        auto rset1 = type->get_args()[0];
        auto rset2 = type->get_args()[1];

        vector<L_expand_element> info;
        TermPtr<int> inner_term;

        // KET-like expansion
        if (rset2->is_atomic()) {
            
            auto reg = args_SUBS_term_reg[1];

            auto [basis, info1] = get_L_expand_info(kernel, reg);

            // create LKET_basis
            ListArgs<int> LKET_basis_args;
            for (const auto& elem : info1) {
                LKET_basis_args.push_back(create_term(LKET, {create_term(elem.bound_var), create_term(elem.reg)})); 
            }
            auto LKET_basis = create_term(LTSR, std::move(LKET_basis_args));

            // create inner term
            inner_term = create_term(SCR, {
                create_term(DOT, {
                    create_term(BRA, {basis}),
                    term->get_args()[0]
                }),
                LKET_basis
            });

            info = std::move(info1);
            
        }

        // BRA-like expansion
        else if (rset1->is_atomic()) {
            
            auto reg = args_SUBS_term_reg[1];

            auto [basis, info2] = get_L_expand_info(kernel, reg);

            // create LBRA_basis
            ListArgs<int> LBRA_basis_args;
            for (const auto& elem : info2) {
                LBRA_basis_args.push_back(create_term(LBRA, {create_term(elem.bound_var), create_term(elem.reg)})); 
            }
            auto LBRA_basis = create_term(LTSR, std::move(LBRA_basis_args));

            // create inner term
            inner_term = create_term(SCR, {
                create_term(DOT, {
                    term->get_args()[0],
                    create_term(KET, {basis})
                }),
                LBRA_basis
            });

            info = std::move(info2);
        }

        // OPT-like expansion
        else {
            
            auto reg1 = args_SUBS_term_reg[1];
            auto reg2 = args_SUBS_term_reg[2];

            auto [basis1, info1] = get_L_expand_info(kernel, reg1);
            auto [basis2, info2] = get_L_expand_info(kernel, reg2);

            // create LBRA_basis
            ListArgs<int> LKET_basis_args;
            for (const auto& elem : info1) {
                LKET_basis_args.push_back(create_term(LKET, {create_term(elem.bound_var), create_term(elem.reg)})); 
            }
            auto LKET_basis = create_term(LTSR, std::move(LKET_basis_args));

            ListArgs<int> LBRA_basis_args;
            for (const auto& elem : info2) {
                LBRA_basis_args.push_back(create_term(LBRA, {create_term(elem.bound_var), create_term(elem.reg)})); 
            }
            auto LBRA_basis = create_term(LTSR, std::move(LBRA_basis_args));

            // create inner term
            inner_term = create_term(SCR, {
                create_term(DOT, {
                    create_term(BRA, {basis1}),
                    create_term(MULK, {
                        term->get_args()[0],
                        create_term(KET, {basis2})
                    })
                }),
                create_term(LDOT, {
                    LKET_basis,
                    LBRA_basis
                })
            });

            info1.insert(info1.end(), info2.begin(), info2.end());
            info = std::move(info1);

        }
    
        // wrap with SUM
        for (int i = info.size() - 1; i >= 0; i--) {
            inner_term = create_term(SUM, {
                create_term(USET, {info[i].index}),
                create_term(FUN, {
                    create_term(info[i].bound_var),
                    create_term(BASIS, {info[i].index}),
                    inner_term
                })
            });
        }

        return inner_term;
    }

    // ADJ(LTSR(D1 D2 ... Dn)) -> LTSR(ADJ(Dn) ... ADJ(D2) ADJ(D1))
    DIRACOQ_RULE_DEF(R_ADJD0, kernel, term) {

        MATCH_HEAD(term, ADJ, args_ADJ_LTSR_D1_D2_Dn)

        MATCH_HEAD(args_ADJ_LTSR_D1_D2_Dn[0], LTSR, args_LTSR_D1_D2_Dn)

        ListArgs<int> new_args;
        for (int i = args_LTSR_D1_D2_Dn.size() - 1; i >= 0; i--) {
            new_args.push_back(create_term(ADJ, {args_LTSR_D1_D2_Dn[i]}));
        }

        return create_term(LTSR, std::move(new_args));
    }

    // ADJ(LDOT(D1 D2)) -> LDOT(ADJ(D2) ADJ(D1))
    DIRACOQ_RULE_DEF(R_ADJD1, kernel, term) {

        MATCH_HEAD(term, ADJ, args_ADJ_LDOT_D1_D2)

        MATCH_HEAD(args_ADJ_LDOT_D1_D2[0], LDOT, args_LDOT_D1_D2)

        return create_term(LDOT, 
            {
                create_term(ADJ, {args_LDOT_D1_D2[1]}),
                create_term(ADJ, {args_LDOT_D1_D2[0]})
            }
        );
    }

    // LTSR(D1 ... SCR(a Dn) ... Dm) -> SCR(a LTSR(D1 ... Dn ... Dm))
    DIRACOQ_RULE_DEF(R_SCRD0, kernel, term) {

        MATCH_HEAD(term, LTSR, args_LTSR_D1_SCR_a_Dn_Dm)

        auto it = std::find_if(args_LTSR_D1_SCR_a_Dn_Dm.begin(), args_LTSR_D1_SCR_a_Dn_Dm.end(), 
            [](const auto& arg) { return arg->get_head() == SCR; });

        if (it == args_LTSR_D1_SCR_a_Dn_Dm.end()) return std::nullopt;

        auto a = (*it)->get_args()[0];
        auto Dn = (*it)->get_args()[1];

        ListArgs<int> new_args;

        // use iterator to iterate and compare with it
        for (auto it2 = args_LTSR_D1_SCR_a_Dn_Dm.begin(); it2 != args_LTSR_D1_SCR_a_Dn_Dm.end(); it2++) {
            if (it2 == it) {
                new_args.push_back(Dn);
            }
            else {
                new_args.push_back(*it2);
            }
        }

        return create_term(SCR, {a, create_term(LTSR, std::move(new_args))});
    }

    // LDOT(SCR(a D1) D2) -> SCR(a LDOT(D1 D2))
    DIRACOQ_RULE_DEF(R_SCRD1, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_SCR_a_D1_D2)

        MATCH_HEAD(args_LDOT_SCR_a_D1_D2[0], SCR, args_SCR_a_D1)

        return create_term(SCR, 
            {
                args_SCR_a_D1[0],
                create_term(LDOT, {args_SCR_a_D1[1], args_LDOT_SCR_a_D1_D2[1]})
            }
        );
    }

    // LDOT(D1 SCR(a D2)) -> SCR(a LDOT(D1 D2))
    DIRACOQ_RULE_DEF(R_SCRD2, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_D1_SCR_a_D2)

        MATCH_HEAD(args_LDOT_D1_SCR_a_D2[1], SCR, args_SCR_a_D2)

        return create_term(SCR, 
            {
                args_SCR_a_D2[0],
                create_term(LDOT, {args_LDOT_D1_SCR_a_D2[0], args_SCR_a_D2[1]})
            }
        );
    }

    // LTSR(X1 ... ADD(D1 ... Dn) ... Xm) -> ADD(LTSR(X1 ... D1 ... Xm) ... LTSR(X1 ... Dn ... Xm))
    DIRACOQ_RULE_DEF(R_TSRD0, kernel, term) {

        MATCH_HEAD(term, LTSR, args_LTSR_X1_ADD_D1_Dn_Xm)

        auto it = std::find_if(args_LTSR_X1_ADD_D1_Dn_Xm.begin(), args_LTSR_X1_ADD_D1_Dn_Xm.end(), 
            [](const auto& arg) { return arg->get_head() == ADD; });

        if (it == args_LTSR_X1_ADD_D1_Dn_Xm.end()) return std::nullopt;

        ListArgs<int> new_args;

        // use iterator to iterate and compare with it
        for (int i = 0; i < (*it)->get_args().size(); i++) {
            ListArgs<int> LTSR_args;
            for (auto it2 = args_LTSR_X1_ADD_D1_Dn_Xm.begin(); it2 != args_LTSR_X1_ADD_D1_Dn_Xm.end(); it2++) {
                if (it2 == it) {
                    LTSR_args.push_back((*it)->get_args()[i]);
                }
                else {
                    LTSR_args.push_back(*it2);
                }
            }
            new_args.push_back(create_term(LTSR, std::move(LTSR_args)));
        }

        return create_term(ADD, std::move(new_args));
    }

    // LDOT(ADD(X1 ... Xn) Y) -> ADD(LDOT(X1 Y) ... LDOT(Xn Y))
    DIRACOQ_RULE_DEF(R_DOTD0, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_ADD_X1_Xn_Y)

        MATCH_HEAD(args_LDOT_ADD_X1_Xn_Y[0], ADD, args_ADD_X1_Xn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(create_term(LDOT, {arg, args_LDOT_ADD_X1_Xn_Y[1]}));
        }

        return create_term(ADD, std::move(new_args));
    }

    // LDOT(Y ADD(X1 ... Xn)) -> ADD(LDOT(Y X1) ... LDOT(Y Xn))
    DIRACOQ_RULE_DEF(R_DOTD1, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_Y_ADD_X1_Xn)

        MATCH_HEAD(args_LDOT_Y_ADD_X1_Xn[1], ADD, args_ADD_X1_Xn)

        ListArgs<int> new_args;
        for (const auto& arg : args_ADD_X1_Xn) {
            new_args.push_back(create_term(LDOT, {args_LDOT_Y_ADD_X1_Xn[0], arg}));
        }

        return create_term(ADD, std::move(new_args));
    }

    // LTSR(X1 ... SUM(M FUN(i T Y)) ... Xn) -> SUM(M FUN(i T LTSR(X1 ... Y ... Xn)))
    DIRACOQ_RULE_DEF(R_SUM_PUSHD0, kernel, term) {

        MATCH_HEAD(term, LTSR, args_LTSR_X1_SUM_M_fun_i_T_Y_Xn)

        auto it = std::find_if(args_LTSR_X1_SUM_M_fun_i_T_Y_Xn.begin(), args_LTSR_X1_SUM_M_fun_i_T_Y_Xn.end(), 
            [](const auto& arg) { return arg->get_head() == SUM && arg->get_args()[1]->get_head() == FUN; });

        if (it == args_LTSR_X1_SUM_M_fun_i_T_Y_Xn.end()) return std::nullopt;

        auto M = (*it)->get_args()[0];
        auto i = (*it)->get_args()[1]->get_args()[0];
        auto T = (*it)->get_args()[1]->get_args()[1];
        auto Y = (*it)->get_args()[1]->get_args()[2];

        ListArgs<int> new_args;

        // use iterator to iterate and compare with it
        for (auto it2 = args_LTSR_X1_SUM_M_fun_i_T_Y_Xn.begin(); it2 != args_LTSR_X1_SUM_M_fun_i_T_Y_Xn.end(); it2++) {
            if (it2 == it) {
                new_args.push_back(Y);
            }
            else {
                new_args.push_back(*it2);
            }
        }

        return create_term(SUM, 
            {
                M,
                create_term(FUN, 
                    {
                        i,
                        T,
                        create_term(LTSR, std::move(new_args))
                    }
                )
            }
        );
        
    }


    // LDOT(SUM(M FUN(i T Y)) X) -> SUM(M FUN(i T LDOT(Y X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSHD1, kernel, term) {

        MATCH_HEAD(term, LDOT, args_LDOT_SUM_M_fun_i_T_Y_X)

        MATCH_HEAD(args_LDOT_SUM_M_fun_i_T_Y_X[0], SUM, args_SUM_M_fun_i_T_Y)

        MATCH_HEAD(args_SUM_M_fun_i_T_Y[1], FUN, args_FUN_i_T_Y)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_Y[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_Y[0],
                        args_FUN_i_T_Y[1],
                        create_term(LDOT, {args_FUN_i_T_Y[2], args_LDOT_SUM_M_fun_i_T_Y_X[1]})
                    }
                )
            }
        );
    }

    // LDOT(Y SUM(M FUN(i T X)) -> SUM(M FUN(i T LDOT(Y X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSHD2, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_Y_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_LDOT_Y_SUM_M_fun_i_T_X[1], SUM, args_SUM_M_fun_i_T_X)

        MATCH_HEAD(args_SUM_M_fun_i_T_X[1], FUN, args_FUN_i_T_X)

        return create_term(SUM, 
            {
                args_SUM_M_fun_i_T_X[0],
                create_term(FUN, 
                    {
                        args_FUN_i_T_X[0],
                        args_FUN_i_T_X[1],
                        create_term(LDOT, {args_LDOT_Y_SUM_M_fun_i_T_X[0], args_FUN_i_T_X[2]})
                    }
                )
            }
        );
    }

    // A : DTYPE[s1, s2], B : DTYPE[s3, s4], s2 || s3 => LDOT(A B) -> LTSR(A B)
    DIRACOQ_RULE_DEF(R_L_SORT0, kernel, term) {

        MATCH_HEAD(term, LDOT, args_LDOT_A_B)

        auto type_A = kernel.calc_type(args_LDOT_A_B[0]);
        auto type_B = kernel.calc_type(args_LDOT_A_B[1]);

        if (type_A->get_head() != DTYPE || type_B->get_head() != DTYPE) return std::nullopt;

        auto s2 = type_A->get_args()[1];
        auto s3 = type_B->get_args()[0];

        if (!rset_disjoint(s2, s3)) return std::nullopt;

        return create_term(LTSR, args_LDOT_A_B);
    }

    // LDOT(LBRA(i, r), LKET(j, r)) -> DELTA(i j)
    DIRACOQ_RULE_DEF(R_L_SORT1, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_LBRA_i_r_LKET_j_r)

        MATCH_HEAD(args_LDOT_LBRA_i_r_LKET_j_r[0], LBRA, args_LBRA_i_r)

        MATCH_HEAD(args_LDOT_LBRA_i_r_LKET_j_r[1], LKET, args_LKET_j_r)

        // check if r is the same
        if (args_LBRA_i_r[1]->get_head() != args_LKET_j_r[1]->get_head()) return std::nullopt;

        return create_term(DELTA, {args_LBRA_i_r[0], args_LKET_j_r[0]});
    }

    // LDOT(LBRA(i, r), LTSR(Y1 ... LKET(j, r) ... Yn)) -> SCR(DELTA(i j) LTSR(Y1 ... Yn))
    DIRACOQ_RULE_DEF(R_L_SORT2, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_LBRA_i_r_LTSR_Y1_LKET_j_r_Yn)

        MATCH_HEAD(args_LDOT_LBRA_i_r_LTSR_Y1_LKET_j_r_Yn[0], LBRA, args_LBRA_i_r)

        MATCH_HEAD(args_LDOT_LBRA_i_r_LTSR_Y1_LKET_j_r_Yn[1], LTSR, args_LTSR_Y1_LKET_j_r_Yn)

        if (args_LTSR_Y1_LKET_j_r_Yn.size() < 2) return std::nullopt;

        auto it = std::find_if(args_LTSR_Y1_LKET_j_r_Yn.begin(), args_LTSR_Y1_LKET_j_r_Yn.end(), 
            [&](const auto& arg) { return arg->get_head() == LKET && arg->get_args()[1]->get_head() == args_LBRA_i_r[1]->get_head(); });

        if (it == args_LTSR_Y1_LKET_j_r_Yn.end()) return std::nullopt;

        auto j = (*it)->get_args()[0];

        ListArgs<int> new_args;

        // use iterator to iterate and compare with it
        for (auto it2 = args_LTSR_Y1_LKET_j_r_Yn.begin(); it2 != args_LTSR_Y1_LKET_j_r_Yn.end(); it2++) {
            if (it2 == it) {
                continue;
            }
            new_args.push_back(*it2);
        }

        return create_term(SCR, {create_term(DELTA, {args_LBRA_i_r[0], j}), create_term(LTSR, std::move(new_args))});
    }

    // LDOT(LTSR(X1 ... LBRA(i, r) ... Xn), LKET(j, r)) -> SCR(DELTA(i j) LTSR(X1 ... Xn))
    DIRACOQ_RULE_DEF(R_L_SORT3, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_LTSR_X1_LBRA_i_r_Xn_LKET_j_r)


        MATCH_HEAD(args_LDOT_LTSR_X1_LBRA_i_r_Xn_LKET_j_r[1], LKET, args_LKET_j_r)

        MATCH_HEAD(args_LDOT_LTSR_X1_LBRA_i_r_Xn_LKET_j_r[0], LTSR, args_LTSR_X1_LBRA_i_r_Xn)

        if (args_LTSR_X1_LBRA_i_r_Xn.size() < 2) return std::nullopt;

        auto it = std::find_if(args_LTSR_X1_LBRA_i_r_Xn.begin(), args_LTSR_X1_LBRA_i_r_Xn.end(), 
            [&](const auto& arg) { return arg->get_head() == LBRA && arg->get_args()[1]->get_head() == args_LKET_j_r[1]->get_head(); });

        if (it == args_LTSR_X1_LBRA_i_r_Xn.end()) return std::nullopt;

        auto i = (*it)->get_args()[0];

        ListArgs<int> new_args;

        // use iterator to iterate and compare with it
        for (auto it2 = args_LTSR_X1_LBRA_i_r_Xn.begin(); it2 != args_LTSR_X1_LBRA_i_r_Xn.end(); it2++) {
            if (it2 == it) {
                continue;
            }
            new_args.push_back(*it2);
        }

        return create_term(SCR, {create_term(DELTA, {i, args_LKET_j_r[0]}), create_term(LTSR, std::move(new_args))});
    }

    // LDOT(LTSR(X1 ... LBRA(i, r) ... Xn), LTSR(Y1 ... LKET(j, r) ... Yn)) -> SCR(DELTA(i j) LDOT(LTSR(X1 ... Xn) LTSR(Y1 ... Yn)))
    DIRACOQ_RULE_DEF(R_L_SORT4, kernel, term) {
        
        MATCH_HEAD(term, LDOT, args_LDOT_LTSR_X1_LBRA_i_r_Xn_LTSR_Y1_LKET_j_r_Yn)

        MATCH_HEAD(args_LDOT_LTSR_X1_LBRA_i_r_Xn_LTSR_Y1_LKET_j_r_Yn[0], LTSR, args_LTSR_X1_LBRA_i_r_Xn)

        MATCH_HEAD(args_LDOT_LTSR_X1_LBRA_i_r_Xn_LTSR_Y1_LKET_j_r_Yn[1], LTSR, args_LTSR_Y1_LKET_j_r_Yn)

        if (args_LTSR_X1_LBRA_i_r_Xn.size() < 2 || args_LTSR_Y1_LKET_j_r_Yn.size() < 2) return std::nullopt;

        // find pairing i and j
        bool found_pair = false;
        int i = 0, j = 0;
        for (i = 0; i < args_LTSR_X1_LBRA_i_r_Xn.size(); ++i) {
            if (args_LTSR_X1_LBRA_i_r_Xn[i]->get_head() == LBRA) {
                for (j = 0; j < args_LTSR_Y1_LKET_j_r_Yn.size(); ++j) {
                    if (args_LTSR_Y1_LKET_j_r_Yn[j]->get_head() == LKET) {
                        if (args_LTSR_X1_LBRA_i_r_Xn[i]->get_args()[1]->get_head() == args_LTSR_Y1_LKET_j_r_Yn[j]->get_args()[1]->get_head()) {
                            found_pair = true;
                            break;
                        }
                    }
                }
            }
            if (found_pair) break;
        }

        if (!found_pair) return std::nullopt;

        ListArgs<int> new_args1;
        for (int i2 = 0; i2 < args_LTSR_X1_LBRA_i_r_Xn.size(); i2++) {
            if (i2 == i) {
                continue;
            }
            else {
                new_args1.push_back(args_LTSR_X1_LBRA_i_r_Xn[i2]);
            }
        }


        ListArgs<int> new_args2;
        for (int j2 = 0; j2 < args_LTSR_Y1_LKET_j_r_Yn.size(); j2++) {
            if (j2 == j) {
                continue;
            }
            else {
                new_args2.push_back(args_LTSR_Y1_LKET_j_r_Yn[j2]);
            }
        }

        return create_term(SCR, {
            create_term(DELTA, {args_LTSR_X1_LBRA_i_r_Xn[i]->get_args()[0], args_LTSR_Y1_LKET_j_r_Yn[j]->get_args()[0]}),
            create_term(LDOT, {create_term(LTSR, std::move(new_args1)), create_term(LTSR, std::move(new_args2))})
        });

    }

    const std::vector<PosRewritingRule> rules = {

        // pre processing rules
        R_COMPO_SS, R_COMPO_SK, R_COMPO_SB, R_COMPO_SO,
        R_COMPO_KS, R_COMPO_KK, R_COMPO_KB,
        R_COMPO_BS, R_COMPO_BK, R_COMPO_BB, R_COMPO_BO,
        R_COMPO_OS, R_COMPO_OK,             R_COMPO_OO,
        R_COMPO_DD,
        R_COMPO_ARROW, R_COMPO_FORALL,
        R_STAR_PROD, R_STAR_MULS, R_STAR_TSRO, R_STAR_CATPROD, R_STAR_LTSR,
        R_ADDG_ADDS, R_ADDG_ADD,
        R_SSUM, 

        // reduction rules
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

        R_SUM_ADDS0, R_SUM_ADD0, R_SUM_ADD1, R_SUM_INDEX0, R_SUM_INDEX1,

        // Qubit rules
        R_QBIT_DELTA, R_QBIT_ONEO, R_QBIT_SUM,

        // Labelled Dirac rules
        R_LABEL_EXPAND, R_ADJD0, R_ADJD1, R_SCRD0, R_SCRD1, R_SCRD2,
        R_TSRD0, R_DOTD0, R_DOTD1, R_SUM_PUSHD0, R_SUM_PUSHD1, R_SUM_PUSHD2,
        R_L_SORT0, R_L_SORT1, R_L_SORT2, R_L_SORT3, R_L_SORT4
    };

    const std::vector<PosRewritingRule> rules_with_wolfram = {

        // pre processing rules
        R_COMPO_SS, R_COMPO_SK, R_COMPO_SB, R_COMPO_SO,
        R_COMPO_KS, R_COMPO_KK, R_COMPO_KB,
        R_COMPO_BS, R_COMPO_BK, R_COMPO_BB, R_COMPO_BO,
        R_COMPO_OS, R_COMPO_OK,             R_COMPO_OO,
        R_COMPO_DD,
        R_COMPO_ARROW, R_COMPO_FORALL,
        R_STAR_PROD, R_STAR_MULS, R_STAR_TSRO, R_STAR_CATPROD, R_STAR_LTSR,
        R_ADDG_ADDS, R_ADDG_ADD,
        R_SSUM, 

        // reduction rules
        R_BETA_ARROW, R_BETA_INDEX, R_DELTA, R_FLATTEN,

        R_MULS2,    // This rules is still necessary because FullSimplify will not transform a * (b + c) to a * b + a * c

        R_CONJ5, R_CONJ6,
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

        R_SUM_ADDS0, R_SUM_ADD0, R_SUM_ADD1, R_SUM_INDEX0, R_SUM_INDEX1,

        // Qubit rules
        R_QBIT_DELTA, R_QBIT_ONEO, R_QBIT_SUM,

        // Labelled Dirac rules
        R_LABEL_EXPAND, R_ADJD0, R_ADJD1, R_SCRD0, R_SCRD1, R_SCRD2,
        R_TSRD0, R_DOTD0, R_DOTD1, R_SUM_PUSHD0, R_SUM_PUSHD1, R_SUM_PUSHD2,
        R_L_SORT0, R_L_SORT1, R_L_SORT2, R_L_SORT3, R_L_SORT4
    };

} // namespace diracoq
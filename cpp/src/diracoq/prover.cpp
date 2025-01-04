#include "diracoq.hpp"

namespace diracoq {
    using namespace std;
    using namespace ualg;

    bool Prover::process(const astparser::AST& ast) {
        // GROUP ( ... )
        try {
            if (ast.head == "GROUP") {
                for (const auto& cmd : ast.children) {
                    if (!process(cmd)){
                        return false;
                    }
                }
                return true;
            }
            else if (ast.head == "DEF") {
                // DEF(x t)
                if (ast.children.size() == 2) {
                    if (!check_id(ast.children[0])) return false;

                    auto name = ast.children[0].head;
                    auto term = kernel.parse(ast.children[1]);
                    kernel.def(kernel.register_symbol(name), term);

                    return true;
                }
                // DEF(x t T)
                if (ast.children.size() == 3) {
                    if (!check_id(ast.children[0])) return false;

                    auto name = ast.children[0].head;
                    auto term = kernel.parse(ast.children[1]);
                    auto type = kernel.parse(ast.children[2]);
                    kernel.def(kernel.register_symbol(name), term, type);

                    return true;
                }
                else {
                    output << "Error: the definition is not valid." << endl;
                    output << "In the command: " << ast.to_string() << endl;
                    return false;
                }
            }
            else if (ast.head == "VAR")  {
                // Assum(x T)
                if (ast.children.size() == 2) {
                    if (!check_id(ast.children[0])) return false;

                    
                    auto name = ast.children[0].head;
                    auto type = kernel.parse(ast.children[1]);
                    kernel.assum(kernel.register_symbol(name), type);

                    return true;
                }
                else {
                    output << "Error: the assumption is not valid." << endl;
                    output << "In the command: " << ast.to_string() << endl;
                    return false;
                }
            }
            else if (ast.head == "CHECK") {
                // CHECK(x)
                if (ast.children.size() == 1) {
                    try {
                        auto term = kernel.parse(ast.children[0]);
                        auto type = kernel.calc_type(term);
                        output << kernel.term_to_string(term) << " : " << kernel.term_to_string(type) << endl;

                        return true;
                    }
                    catch (const exception& e) {
                        output << "Error: " << e.what() << endl;
                        return false;
                    }
                }
            }
            // SHOW(x)
            else if (ast.head == "SHOW") {
                if (ast.children.size() == 1) {
                    if (!check_id(ast.children[0])) return false;

                    auto name = ast.children[0].head;
                    try {
                        // get the definition in the env
                        auto find_def = kernel.find_in_env(kernel.register_symbol(name));
                        if (find_def == nullopt) {
                            output << "Error: the symbol '" << name << "' is not defined." << endl;
                            return false;
                        }
                        output << kernel.dec_to_string(name, find_def.value()) << endl;

                        return true;
                    }
                    catch (const exception& e) {
                        output << "Error: " << e.what() << endl;
                        return false;
                    }
                }
            }
            // SHOWALL
            else if (ast.head == "SHOWALL") {
                if (ast.children.size() == 0) {
                    output << "Environment:" << endl;
                    output << kernel.env_to_string() << endl;

                    return true;
                }
                else {
                    output << "Error: the command is not valid." << endl;
                    output << "In the command: " << ast.to_string() << endl;
                    return false;
                }
            }
            else if (ast.head == "NORMALIZE") {
                if (ast.children.size() > 2) {
                    output << "Error: NORMALIZE command should have one or two arguments." << endl;
                    return false;
                }
                if (ast.children.size() == 2) {
                    if (ast.children[1].head != "TRACE") {
                        output << "Error: the second argument of NORMALIZE command should be 'TRACE'." << endl;
                        return false;
                    }
                }

                // Typecheck the term
                auto term = kernel.parse(ast.children[0]);
                auto type = kernel.calc_type(term);

                // calculate the normalized term
                vector<PosReplaceRecord> trace;

                try {
                    // rename to unique variables first
                    auto renamed_res = bound_variable_rename(kernel, term);

                    // first rewriting
                    auto temp = pos_rewrite_repeated(kernel, renamed_res, all_rules, &trace);

                    // NEED TO CONSIDER DIFFERENT PHASES

                    // expand on variables
                    auto expanded_term = variable_expand(kernel, temp);

                    // second rewriting
                    temp = pos_rewrite_repeated(kernel, expanded_term, all_rules, &trace);
                    
                    // cout << "[Normalized]:\t\t\t\t" << kernel.term_to_string(temp) << endl;

                    // calculate the bound variables
                    auto bound_vars = get_bound_vars(temp);

                    // sorting modulo the bound variables
                    temp = sort_C_terms(temp, kernel.get_bank(), c_symbols, 
                        [&](const Term<int>* a, const Term<int>* b) {
                            return comp_modulo_bound_vars(a, b, bound_vars);
                        }
                    );

                    // cout << "[Sorted    ]:\t\t\t\t" << kernel.term_to_string(temp) << endl;

                    // reduce to sum_swap normal form
                    temp = sum_swap_normalization(kernel, temp);

                    // cout << "[Sum Swap  ]:\t\t\t\t" << kernel.term_to_string(temp) << endl;

                    auto normalized_term = deBruijn_normalize(kernel, temp);

                    auto final_term = normalized_term;

                    // if output trace
                    if (ast.children.size() == 2) {
                        output << "Trace:" << endl;
                        for (int i = 0; i < trace.size(); ++i) {
                            output << "Step " + to_string(i) << endl;
                            output << kernel.term_to_string(trace[i].init_term) << endl;
                            output << pos_replace_record_to_string(kernel, trace[i]) << endl;
                        }
                    }
                    
                    // Output the normalized term
                    output << kernel.term_to_string(final_term) + " : " + kernel.term_to_string(type)  << endl;

                    return true;

                }
                catch (const exception& e) {
                    // output the trace first
                    output << "Trace:" << endl;
                    for (int i = 0; i < trace.size(); ++i) {
                        output << "Step " + to_string(i) << endl;
                        output << kernel.term_to_string(trace[i].init_term) << endl;
                        output << pos_replace_record_to_string(kernel, trace[i]) << endl;
                    }

                    throw;
                }

            }
            else if (ast.head == "CHECKEQ") {
                if (ast.children.size() != 2) {
                    output << "Error: CHECKEQ command should have two arguments." << endl;
                    return false;
                }
                check_eq(ast.children[0], ast.children[1]);
                return true;
            }
        }
        catch (const exception& e) {
            output << "Error: " << e.what() << endl;
            return false;
        }

        // bad command
        output << "Error: the command is not valid." << endl;
        output << "In the command: " << ast.to_string() << endl;
        return false;
    }

    bool Prover::check_eq(const astparser::AST& codeA, const astparser::AST& codeB) {
        
        // Typecheck the terms
        auto termA = kernel.parse(codeA);
        auto termB = kernel.parse(codeB);
        auto typeA = kernel.calc_type(termA);
        auto typeB = kernel.calc_type(termB);
        if (!kernel.is_judgemental_eq(typeA, typeB)) {
            output << "The two terms have different types and are not equal." << endl;
            output << "[TYPE A] " << kernel.term_to_string(typeA) << endl;
            output << "[TYPE B] " << kernel.term_to_string(typeB) << endl;
            return false;
        }

        // calculate the normalized term
        vector<PosReplaceRecord> traceA;
        auto renamed_resA = bound_variable_rename(kernel, termA);
        auto tempA = pos_rewrite_repeated(kernel, renamed_resA, all_rules, &traceA);
        auto expanded_termA = variable_expand(kernel, tempA);
        tempA = pos_rewrite_repeated(kernel, expanded_termA, all_rules, &traceA);
        auto bound_varsA = get_bound_vars(tempA);
        tempA = sort_C_terms(tempA, kernel.get_bank(), c_symbols, 
            [&](const Term<int>* a, const Term<int>* b) {
                return comp_modulo_bound_vars(a, b, bound_varsA);
            }
        );
        tempA = sum_swap_normalization(kernel, tempA);
        auto normalized_termA = deBruijn_normalize(kernel, tempA);

        vector<PosReplaceRecord> traceB;
        auto renamed_resB = bound_variable_rename(kernel, termB);
        auto tempB = pos_rewrite_repeated(kernel, renamed_resB, all_rules, &traceB);
        auto expanded_termB = variable_expand(kernel, tempB);
        tempB = pos_rewrite_repeated(kernel, expanded_termB, all_rules, &traceB);
        auto bound_varsB = get_bound_vars(tempB);
        tempB = sort_C_terms(tempB, kernel.get_bank(), c_symbols, 
            [&](const Term<int>* a, const Term<int>* b) {
                return comp_modulo_bound_vars(a, b, bound_varsB);
            }
        );
        tempB = sum_swap_normalization(kernel, tempB);
        auto normalized_termB = deBruijn_normalize(kernel, tempB);

        auto final_termA = normalized_termA;
        auto final_termB = normalized_termB;
        
        // Output the result
        if (final_termA == final_termB) {
            output << "The two terms are equal." << endl;
            output << "[Normalized Term] " << kernel.term_to_string(final_termA) << " : " << kernel.term_to_string(typeA) << endl;

            return true;
        }
        else {
            output << "The two terms are not equal." << endl;
            output << "[Normalized Term A] " << kernel.term_to_string(final_termA) << " : " << kernel.term_to_string(typeA) << endl;
            output << "[Normalized Term B] " << kernel.term_to_string(final_termB) << " : " << kernel.term_to_string(typeB) << endl;
            return false;
        }
    }

    Prover* std_prover() {
        auto res = new Prover{std::cout};

        res->process(R"(
        (* Trace
        DNTr[M_, T_]:=Module[{i}, SUMS[IDX[{i, USET[T]}], Bra[{i}]\[SmallCircle]M\[SmallCircle]Ket[{i}]]];
        *)
        Def Tr := idx T => fun O : OTYPE[T, T] => Sum i in USET[T], <i| O |i>.

        (* SWAP
        SWAP[term_, T1_, T2_, T3_, T4_]:=
	Module[{i, j, k, l},
	SUMO[IDX[{i,USET[T1]}, {j,USET[T2]}, {k,USET[T3]}, {l,USET[T4]}],
		(Bra[{PAIR[i,j]}]\[SmallCircle]term\[SmallCircle]Ket[{PAIR[k,l]}])(Ket[{PAIR[j,i]}]\[SmallCircle]Bra[{PAIR[l,k]}])]];
        *)
        Def SWAP := idx T1 => idx T2 => idx T3 => idx T4 => fun O : OTYPE[T1 * T2, T3 * T4] => Sum i in USET[T1], Sum j in USET[T2], Sum k in USET[T3], Sum l in USET[T4], (<(i, j)| O |(k, l)>).(|(j, i)> <(l, k)|).

        (* Partial Trace 1
        DNPTr1[M_, T_, T1_, T2_]:= 
	Module[{i, j, k}, 
	SUMO[IDX[{i, USET[T1]}, {j, USET[T2]}, {k, USET[T]}], 
		(Bra[{PAIR[k, i]}]\[SmallCircle]M\[SmallCircle]Ket[{PAIR[k, j]}])(Ket[{i}]\[SmallCircle]Bra[{j}])]];
        *)
        Def PTr1 := idx T => idx T1 => idx T2 => fun O : OTYPE[T * T1, T * T2] => Sum i in USET[T1], Sum j in USET[T2], Sum k in USET[T], (<(k, i)| O |(k, j)>).(|i> <j|).

        (* Partial Trace 2
        DNPTr2[M_, T_, T1_, T2_]:=
	Module[{i, j, k},
	SUMO[IDX[{i, USET[T1]}, {j, USET[T2]}, {k, USET[T]}],
		(Bra[{PAIR[i, k]}]\[SmallCircle]M\[SmallCircle]Ket[{PAIR[j, k]}])(Ket[{i}]\[SmallCircle]Bra[{j}])]];
        *)
        Def PTr2 := idx T => idx T1 => idx T2 => fun O : OTYPE[T1 * T, T2 * T] => Sum i in USET[T1], Sum j in USET[T2], Sum k in USET[T], (<(i, k)| O |(j, k)>).(|i> <j|).

        (* Transpose of Ket
        TPK[B_, T_]:= Module[{i}, SUMK[IDX[{i, USET[T]}], (B\[SmallCircle]Ket[{i}])Ket[{i}]]];
        *)
        Def TPK := idx sigma => fun B : BTYPE[sigma] => Sum i in USET[sigma], (B |i>) . |i>.

        (* Transpose of Bra
        TPB[K_, T_]:= Module[{i}, SUMB[IDX[{i, USET[T]}], (Bra[{i}]\[SmallCircle]K)Bra[{i}]]];
        *)
        Def TPB := idx sigma => fun K : KTYPE[sigma] => Sum i in USET[sigma], (<i| K) . <i|.

        (* Transpose of Operator
        TPO[O_, T1_, T2_] := Module[{i, j}, 
	SUMO[IDX[{i, USET[T2]}, {j, USET[T1]}], (Ket[{i}]\[SmallCircle]Bra[{j}])\[SmallCircle]O\[SmallCircle](Ket[{i}]\[SmallCircle]Bra[{j}])]];
        *)
        Def TPO := idx sigma => idx tau => fun O : OTYPE[sigma, tau] => Sum i in USET[sigma], Sum j in USET[tau], (<i| O |j>).(|j> <i|).


        (* Conjugate of Operator 
        CONJO[O_, T1_, T2_] := TPO[ADJO[O],T2,T1];
        *)
        Def CONJO := idx sigma => idx tau => fun O : OTYPE[sigma, tau] => TPO tau sigma O^D.

        (* fromlf 
        formlf[A_, X_]:= A \[SmallCircle] X \[SmallCircle] SuperDagger[A];
        *)
        Def formlf := idx T1 => idx T2 => fun A : OTYPE[T1, T2] => fun X : OTYPE[T2, T2] => A X A^D.

        (* super operator 
        superop[M_, e_, f_]:= Module[{i}, SUMO[IDX[{i, M}], e[i]\[SmallCircle]#\[SmallCircle](SuperDagger[f[i]])]&];
        *)
        Def superop := idx S => fun M : SET[S] => idx T1 => idx T2 => fun e : BASIS[S]->OTYPE[T1, T2] => fun f : BASIS[S]->OTYPE[T1, T2] => fun X : OTYPE[T2, T2] => Sum i in M, (e i) X (f i)^D.

        (* so2choi 
        so2choi[e_, T1_]:=
            Module[
                {i, j},
                SUMO[IDX[{i, USET[T1]}, {j, USET[T1]}], (Ket[{i}]\[SmallCircle]Bra[{j}])\[CircleTimes]e[Ket[{i}]\[SmallCircle]Bra[{j}]]]
            ];
        *)
        Def so2choi := idx T1 => idx T2 => fun E : OTYPE[T1, T1] -> OTYPE[T2, T2] => Sum i in USET[T1], Sum j in USET[T1], (|i> <j|) * (E (|i> <j|)).


        (* krausso
        krausso[M_, f_]:=superop[M,f,f];
        *)
        Def krausso := idx S => fun M : SET[S] => idx T1 => idx T2 => fun f : BASIS[S]->OTYPE[T1, T2] => superop S M T1 T2 f f.
        
        (* idso 
        idso[T_] := (#)&;
        *)
        Def idso := idx T => fun X : OTYPE[T, T] => X.
        
        (* abortso 
        abortso[T1_,T2_] := (ZEROO[T1,T1])&;
        *)
        Def abortso := idx T1 => idx T2 => fun X : OTYPE[T2, T2] => 0O[T1, T1].

        (* fromso
        formso[f_] := (f\[SmallCircle]#\[SmallCircle]SuperDagger[f])&;
        *)
        Def formso := idx T1 => idx T2 => fun f : OTYPE[T1, T2] => fun X : OTYPE[T2, T2] => f X f^D.

        (* addso
        addso[e_, f_]:=(e[#] ~ADDO~ f[#])&; 
        *)
        Def addso := idx T1 => idx T2 => fun e : OTYPE[T2, T2] -> OTYPE[T1, T1] => fun f : OTYPE[T2, T2] -> OTYPE[T1, T1] => fun X : OTYPE[T2, T2] => e X + f X.

        (* sumso 
        sumso[M_,f_]:=Module[{i}, SUMO[i, M, f[i][#]]&];
        *)
        Def sumso := idx S => fun M : SET[S] => idx T1 => idx T2 => fun f : BASIS[S] -> OTYPE[T2, T2] -> OTYPE[T1, T1] => fun X : OTYPE[T2, T2] => Sum i in M, f i X.


        (* scaleso 
        scaleso[c_, e_]:= (c ~SCRO~ e[#])&;
        *)
        Def scaleso := idx T1 => idx T2 => fun c : STYPE => fun E : OTYPE[T2, T2] -> OTYPE[T1, T1] => fun X : OTYPE[T2, T2] => c (E X).

        (* compso 
        compso[e_, f_]:= e[f[#]]&;
        *)
        Def compso := idx T1 => idx T2 => idx T3 => fun e : OTYPE[T2, T2] -> OTYPE[T1, T1] => fun f : OTYPE[T3, T3] -> OTYPE[T2, T2] => fun X : OTYPE[T3, T3] => e (f X).

        (* compsor
        compsor[e_, f_]:= f[e[#]]&;
        *)
        Def compsor := idx T1 => idx T2 => idx T3 => fun e : OTYPE[T3, T3] -> OTYPE[T2, T2] => fun f : OTYPE[T2, T2] -> OTYPE[T1, T1] => fun X : OTYPE[T3, T3] => f (e X).

        (* ifso
        ifso[M_, e_, f_] := Module[{i}, 
	SUMO[IDX[{i, M}], f[i][e[i]\[SmallCircle]#\[SmallCircle]SuperDagger[(e[i])]]]&];
        *)
        Def ifso := idx S => fun M : SET[S] => idx T1 => idx T2 => idx T3 => fun e : BASIS[S] -> OTYPE[T2, T3] => fun F : BASIS[S] -> OTYPE[T2, T2] -> OTYPE[T1, T1] => fun X : OTYPE[T3, T3] => Sum i in M, F i ((e i) X (e i)^D).

        )");

        return res;
    }

} // namespace diracoq
#include "dirace.hpp"

namespace dirace {
    using namespace std;
    using namespace ualg;


    inline TermPtr<int> rewrite_with_wolfram(Kernel& kernel, TermPtr<int> term, vector<PosReplaceRecord>& trace, bool distribute) {
        auto temp = term;

        // use different rules depending on the wolfram connection
        
        if (kernel.wolfram_connected()) {
            while (true) {
                if (distribute) {
                    temp = pos_rewrite_repeated(kernel, temp, rules_with_wolfram_distr, &trace);
                }
                else {
                    temp = pos_rewrite_repeated(kernel, temp, rules_with_wolfram_merge, &trace);
                }

                auto wolfram_simplified = wolfram_fullsimplify(kernel, temp, distribute);

                trace.push_back({
                    "Wolfram Engine",
                    {},
                    temp,
                    nullptr,
                    nullptr,
                    wolfram_simplified
                });

                if (*temp == *wolfram_simplified) {
                    break;
                }

                temp = wolfram_simplified;
            }
            return temp;
        }
        else {
            return pos_rewrite_repeated(kernel, temp, rules, &trace);
        }
    }

    /**
     * @brief Checks if two terms are equal (taking the scalars into consideration) using Wolfram Engine.
     * 
     * @param kernel 
     * @param a 
     * @param b 
     * @return true 
     * @return false 
     */
    bool syntax_eq_with_wolfram(Kernel& kernel, TermPtr<int> a, TermPtr<int> b) {
        using namespace astparser;
        if (*a == *b) return true;

        // try to check by Wolfram Engine
        if (kernel.wolfram_connected()) {
            auto &sig = kernel.get_sig();
            auto request = AST("FullSimplify", {
                AST("Equal", {
                    sig.term2ast(a),
                    sig.term2ast(b)
                })
            });

            wstp::ast_to_WS(kernel.get_wstp_link(), request);

            auto response = wstp::WS_to_ast(kernel.get_wstp_link());

            if (response == AST("True")) {
                return true;
            }
        }

        return false;
    }


    TermPtr<int> normalize(Kernel& kernel, TermPtr<int> term, vector<PosReplaceRecord>& trace, bool distribute) {

        // rename to unique variables first
        auto temp = bound_variable_rename(kernel, term);
        trace.push_back({
            "Bound Variable Rename",
            {},
            term,
            nullptr,
            nullptr,
            temp
        });

        // first rewriting
        temp = rewrite_with_wolfram(kernel, temp, trace, distribute);

        // expand on variables
        temp = variable_expand(kernel, temp);
        trace.push_back({
            "Variable Expand",
            {},
            term,
            nullptr,
            nullptr,
            temp
        });

        // second rewriting
        temp = rewrite_with_wolfram(kernel, temp, trace, distribute);
        
        temp = sort_modulo_bound(kernel, temp);
        trace.push_back({
            "Sort Modulo Bound Variables",
            {},
            term,
            nullptr,
            nullptr,
            temp
        });

        // reduce to sum_swap normal form
        temp = sum_swap_normalization(kernel, temp);
        trace.push_back({
            "Sum Swap Normalization",
            {},
            term,
            nullptr,
            nullptr,
            temp
        });

        auto normalized_term = deBruijn_normalize(kernel, temp);
        trace.push_back({
            "DeBruijn Normalize",
            {},
            term,
            nullptr,
            nullptr,
            normalized_term
        });
        
        return normalized_term;
    }

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

                    auto final_term = normalize(kernel, term, trace, true);

                    // if output trace
                    if (ast.children.size() == 2) {
                        output << "[Trace]" << endl;
                        for (int i = 0; i < trace.size(); ++i) {
                            output << "# " << i << endl;
                            output << record_to_string(kernel, trace[i]) << endl;
                        }
                    }
                    
                    // Output the normalized term
                    output << "[Normal Form]" << kernel.term_to_string(final_term) + " : " + kernel.term_to_string(type)  << endl;

                    return true;

                }
                catch (const exception& e) {
                    // output the trace first
                    if (ast.children.size() == 2) {
                        output << "[Trace]" << endl;
                        for (int i = 0; i < trace.size(); ++i) {
                            output << "# " << i << endl;
                            output << record_to_string(kernel, trace[i]) << endl;
                        }
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
        vector<PosReplaceRecord> traceB;
        TermPtr<int> final_termA;
        TermPtr<int> final_termB;

        final_termA = normalize(kernel, termA, traceA, true);
        final_termB = normalize(kernel, termB, traceB, true);
        
        // Output the result
        if (syntax_eq_with_wolfram(kernel, final_termA, final_termB)) {
            output << "The two terms are equal." << endl;
            output << "[Normalized Term] " << kernel.term_to_string(final_termA) << " : " << kernel.term_to_string(typeA) << endl;
            return true;
        }

        traceA = {};
        traceB = {};
        final_termA = normalize(kernel, termA, traceA, false);
        final_termB = normalize(kernel, termB, traceB, false);

        if (syntax_eq_with_wolfram(kernel, final_termA, final_termB)) {
            output << "The two terms are equal." << endl;
            output << "[Normalized Term] " << kernel.term_to_string(final_termA) << " : " << kernel.term_to_string(typeA) << endl;
            return true;
        }

        output << "The two terms are not equal." << endl;
        output << "[Normalized Term A] " << kernel.term_to_string(final_termA) << " : " << kernel.term_to_string(typeA) << endl;
        output << "[Normalized Term B] " << kernel.term_to_string(final_termB) << " : " << kernel.term_to_string(typeB) << endl;
        return false;
    }

    Prover std_prover(WSLINK wstp_link) {

        auto res = Prover{wstp_link, std::cout};

        res.process(R"(
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
        Def so2choi := idx T1 => idx T2 => fun E : OTYPE[T2, T2] -> OTYPE[T1, T1] => Sum i in USET[T2], Sum j in USET[T2], (|i> <j|) * (E (|i> <j|)).

        (* choi2so
        choi2so[A_, T1_, T2_]:=DNPTr1[A~MLTO~(TPO[#, T1, T1] ~TSRO~ ONEO[T2]), T1, T2, T2]&;
        *)
        Def choi2so := idx T1 => idx T2 => fun A : OTYPE[T2 * T1, T2 * T1] => fun X : OTYPE[T2, T2] => PTr1 T2 T1 T1 (A ((TPO T2 T2 X) * 1O[T1])).

        (* krausso
        krausso[M_, f_]:=superop[M,f,f];
        *)
        Def krausso := idx S => fun M : SET[S] => idx T1 => idx T2 => fun f : BASIS[S]->OTYPE[T1, T2] => superop S M T1 T2 f f.
        
        (* dualso
        dualso[e_, T1_, T2_]:=choi2so[SWAP[TPO[so2choi[e, T1], T1 ~ProdType~ T2, T1 ~ProdType~ T2], T1, T2, T1, T2], T2, T1];
        *)
        Def dualso := idx T2 => idx T1 => fun E : OTYPE[T2, T2] -> OTYPE[T1, T1] => choi2so T2 T1 (SWAP T2 T1 T2 T1 (TPO (T2 * T1) (T2 * T1) (so2choi T1 T2 E))).

        (* elemso
        elemso[f_, k_]:= (f[k]\[SmallCircle]#\[SmallCircle]SuperDagger[f[k]])&;
        *)
        Def elemso := idx S => idx T1 => idx T2 => fun f : BASIS[S] -> OTYPE[T1, T2] => fun k : BASIS[S] => fun X : OTYPE[T2, T2] => (f k) X (f k)^D.

        (* dualqm
        dualqm[M_, f_, X_]:= Module[{i}, SUMO[IDX[{i, M}], (SuperDagger[f[i]])\[SmallCircle]X[i]\[SmallCircle]f[i]]];
        *)
        Def dualqm := idx S => fun M : SET[S] => idx T1 => idx T2 => fun f : BASIS[S] -> OTYPE[T2, T1] => fun X : BASIS[S] -> OTYPE[T2, T2] => Sum i in M, (f i)^D (X i) (f i).

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

        (* oppso 
        oppso[e_] := (CPX[-1] ~SCRO~ e[#])&;
        *)
        Def oppso := idx T1 => idx T2 => fun e : OTYPE[T2, T2] -> OTYPE[T1, T1] => fun X : OTYPE[T2, T2] => -1 . (e X).

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

} // namespace dirace
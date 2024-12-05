#pragma once

#include "ualg.hpp"

namespace diracoq {

    // The rewriting rules of the Diracoq kernel.
    using PosRewritingRule = std::optional<const ualg::Term<int>*> (*)(Kernel& kernel, const ualg::Term<int>* term);

#define DIRACOQ_RULE_DEF(name, kernel, term) std::optional<const ualg::Term<int>*> name(diracoq::Kernel& kernel, const ualg::Term<int>* term)

    // The struct for the rewriting trace. It records which rule is applied, at which position, and the new replacement.
    struct PosReplaceRecord {
        PosRewritingRule rule;
        ualg::NormalTermPos pos;
        const ualg::NormalTerm<int>* replacement;
    };


    /**
     * @brief Get the rewriting record using the rewriting rules.
     * 
     * @param kernel 
     * @param term 
     * @param rules 
     * @return std::optional<PosReplaceRecord> 
     */
    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, const ualg::NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules);


    /**
     * @brief Rewrite the term repeatedly using the given rewriting rules, until no more rules can apply.
     * 
     * @param kernel 
     * @param term 
     * @param rules 
     * @param trace The container to store the trace of the rewriting.
     * @return const NormalTerm<int>* 
     */
    const ualg::NormalTerm<int>* pos_rewrite_repeated(Kernel& kernel, const ualg::NormalTerm<int>* term, const std::vector<PosRewritingRule>& rules, 
    std::vector<PosReplaceRecord>* trace = nullptr);



    /**
     * @brief Iterate through the whole term and rename the bound variables.
     * 
     * This function should be used in the end of the rewriting process, on the whole term, to ensure that the bound variables are correctly renamed.
     * 
     * @param kernel 
     * @param term 
     * @return const ualg::NormalTerm<int>* 
     */
    const ualg::NormalTerm<int>* alpha_normalize(Kernel& kernel, const ualg::NormalTerm<int>* term);

    //////////////// Rules

    // beta reduction
    DIRACOQ_RULE_DEF(R_BETA, kernel, term);
    // delta reduction
    DIRACOQ_RULE_DEF(R_DELTA, kernel, term);
    // eta reduction
    DIRACOQ_RULE_DEF(R_ETA, kernel, term);


    //////////////// Flattening AC symbols
    DIRACOQ_RULE_DEF(R_FLATTEN, kernel, term);


    //////////////// properties of the symbols

    // ADDS(a) -> a
    DIRACOQ_RULE_DEF(R_ADDSID, kernel, term);
    // MULS(a) -> a
    DIRACOQ_RULE_DEF(R_MULSID, kernel, term);


    //////////////// rewriting rules

    // ADDS(a 0) -> a
    // This rule removes all 0s from the subterm
    DIRACOQ_RULE_DEF(R_ADDS0, kernel, term);

    // MULS(a 0) -> 0
    DIRACOQ_RULE_DEF(R_MULS0, kernel, term);

    // MULS(a 1) -> a
    // This rule removes all 0s from the subterm
    DIRACOQ_RULE_DEF(R_MULS1, kernel, term);

    // MULS((seq1: __) ADDS(a1 a2 ... an) (seq2: __)) -> ADDS(MULS(seq1 a1 seq2) MULS(seq1 a2 seq2) ... MULS(seq1 an seq2))
    // This rule expands on the first ocurrence of ADDS in the subterm
    DIRACOQ_RULE_DEF(R_MULS2, kernel, term);

    // CONJ(0) -> 0
    DIRACOQ_RULE_DEF(R_CONJ0, kernel, term);

    // CONJ(1) -> 1
    DIRACOQ_RULE_DEF(R_CONJ1, kernel, term);

    // CONJ(ADDS(a b)) -> ADDS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ2, kernel, term);

    // CONJ(MULS(a b)) -> MULS(CONJ(a) CONJ(b))
    DIRACOQ_RULE_DEF(R_CONJ3, kernel, term);

    // CONJ(CONJ(a)) -> a
    DIRACOQ_RULE_DEF(R_CONJ4, kernel, term);

    // CONJ(DELTA(s t)) -> DELTA(s t)
    DIRACOQ_RULE_DEF(R_CONJ5, kernel, term);

    // CONJ(DOT(B K)) -> DOT(ADJ(K) ADJ(B))
    DIRACOQ_RULE_DEF(R_CONJ6, kernel, term);

    // DOT(0B(sigma) K) -> 0
    DIRACOQ_RULE_DEF(R_DOT0, kernel, term);

    // DOT(B 0K(sigma)) -> 0
    DIRACOQ_RULE_DEF(R_DOT1, kernel, term);

    // DOT(SCR(a B) K) -> MULS(a DOT(B K))
    DIRACOQ_RULE_DEF(R_DOT2, kernel, term);

    // DOT(B SCR(a K)) -> MULS(a DOT(B K))
    DIRACOQ_RULE_DEF(R_DOT3, kernel, term);

    // DOT(ADD(B1 ... Bn) K) -> ADD(DOT(B1 K) ... DOT(Bn K))
    DIRACOQ_RULE_DEF(R_DOT4, kernel, term);

    // DOT(B ADD(K1 ... Kn)) -> ADD(DOT(B K1) ... DOT(B Kn))
    DIRACOQ_RULE_DEF(R_DOT5, kernel, term);

    // DOT(BRA(s) KET(t)) -> DELTA(s t)
    DIRACOQ_RULE_DEF(R_DOT6, kernel, term);

    // DOT(TSR(B1 B2) KET(PAIR(s t))) -> MULS(DOT(B1 KET(s)) DOT(B2 KET(t)))
    DIRACOQ_RULE_DEF(R_DOT7, kernel, term);

    // DOT(BRA(PAIR(s t)) TSR(K1 K2)) -> MULS(DOT(BRA(s) K1) DOT(BRA(t) K2))
    DIRACOQ_RULE_DEF(R_DOT8, kernel, term);

    // DOT(TSR(B1 B2) TSR(K1 K2)) -> MULS(DOT(B1 K1) DOT(B2 K2))
    DIRACOQ_RULE_DEF(R_DOT9, kernel, term);

    // DOT(MULB(B O) K) -> DOT(B MULK(O K))
    DIRACOQ_RULE_DEF(R_DOT10, kernel, term);

    // DOT(BRA(PAIR(s t)) MULK(TSR(O1 O2) K)) -> DOT(TSR(MULB(BRA(s) O1) MULB(BRA(t) O2)) K)
    DIRACOQ_RULE_DEF(R_DOT11, kernel, term);

    // DOT(TSR(B1 B2) MULK(TSR(O1 O2) K)) -> DOT(TSR(MULB(B1 O1) MULB(B2 O2)) K)
    DIRACOQ_RULE_DEF(R_DOT12, kernel, term);

    // DELTA(a a) -> 1
    DIRACOQ_RULE_DEF(R_DELTA0, kernel, term);

    // DELTA(PAIR(a b) PAIR(c d)) -> MULS(DELTA(a c) DELTA(b d))
    DIRACOQ_RULE_DEF(R_DELTA1, kernel, term);
    
    // SCR(1 X) -> X
    DIRACOQ_RULE_DEF(R_SCR0, kernel, term);

    // SCR(a SCR(b X)) -> SCR(MULS(a b) X)
    DIRACOQ_RULE_DEF(R_SCR1, kernel, term);

    // SCR(a ADD(X1 ... Xn)) -> ADD(SCR(a X1) ... SCR(a Xn))
    DIRACOQ_RULE_DEF(R_SCR2, kernel, term);

    // K : KType(T) => SCR(0 K) -> 0K(T)
    DIRACOQ_RULE_DEF(R_SCRK0, kernel, term);

    // SCR(a 0K(T)) -> 0K(T)
    DIRACOQ_RULE_DEF(R_SCRK1, kernel, term);

    // B : BType(T) => SCR(0 B) -> 0B(T)
    DIRACOQ_RULE_DEF(R_SCRB0, kernel, term);

    // SCR(a 0B(T)) -> 0B(T)
    DIRACOQ_RULE_DEF(R_SCRB1, kernel, term);

    // O : OType(T1 T2) => SCR(0 O) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_SCRO0, kernel, term);

    // SCR(a 0O(T1 T2)) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_SCRO1, kernel, term);

    // ADD(X) -> X
    DIRACOQ_RULE_DEF(R_ADDID, kernel, term);

    // ADD(Y1 ... X ... X ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(1 1) X))
    DIRACOQ_RULE_DEF(R_ADD0, kernel, term);

    // ADD(Y1 ... X ... SCR(a X) ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(1 a) X))
    DIRACOQ_RULE_DEF(R_ADD1, kernel, term);

    // ADD(Y1 ... SCR(a X) ... X ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(a 1) X))
    DIRACOQ_RULE_DEF(R_ADD2, kernel, term);

    // ADD(Y1 ... SCR(a X) ... SCR(b X) ... Yn) -> ADD(Y1 ... Yn SCR(ADDS(a b) X))
    DIRACOQ_RULE_DEF(R_ADD3, kernel, term);

    // ADD(K1 ... 0K(T) ... Kn) -> ADD(K1 ... Kn)
    DIRACOQ_RULE_DEF(R_ADDK0, kernel, term);

    // ADD(B1 ... 0B(T) ... Bn) -> ADD(B1 ... Bn)
    DIRACOQ_RULE_DEF(R_ADDB0, kernel, term);

    // ADD(O1 ... 0O(T1 T2) ... On) -> ADD(O1 ... On)
    DIRACOQ_RULE_DEF(R_ADDO0, kernel, term);

    // ADJ(ADJ(X)) -> X
    DIRACOQ_RULE_DEF(R_ADJ0, kernel, term);

    // ADJ(SCR(a X)) -> SCR(CONJ(a) ADJ(X))
    DIRACOQ_RULE_DEF(R_ADJ1, kernel, term);

    // ADJ(ADD(X1 ... Xn)) -> ADD(ADJ(X1) ... ADJ(Xn))
    DIRACOQ_RULE_DEF(R_ADJ2, kernel, term);

    // ADJ(TSR(X Y)) -> TSR(ADJ(X) ADJ(Y))
    DIRACOQ_RULE_DEF(R_ADJ3, kernel, term);

    // ADJ(0B(T)) -> 0K(T)
    DIRACOQ_RULE_DEF(R_ADJK0, kernel, term);

    // ADJ(BRA(t)) -> KET(t)
    DIRACOQ_RULE_DEF(R_ADJK1, kernel, term);

    // ADJ(MULB(B O)) -> MULK(ADJ(O) ADJ(B))
    DIRACOQ_RULE_DEF(R_ADJK2, kernel, term);

    // ADJ(0K(T)) -> 0B(T)
    DIRACOQ_RULE_DEF(R_ADJB0, kernel, term);

    // ADJ(KET(t)) -> BRA(t)
    DIRACOQ_RULE_DEF(R_ADJB1, kernel, term);

    // ADJ(MULK(O K)) -> MULB(ADJ(K) ADJ(O))
    DIRACOQ_RULE_DEF(R_ADJB2, kernel, term);

    // ADJ(0O(T1 T2)) -> 0O(T2 T1)
    DIRACOQ_RULE_DEF(R_ADJO0, kernel, term);

    // ADJ(1O(T)) -> 1O(T)
    DIRACOQ_RULE_DEF(R_ADJO1, kernel, term);

    // ADJ(OUTER(K B)) -> OUTER(ADJ(B) ADJ(K))
    DIRACOQ_RULE_DEF(R_ADJO2, kernel, term);

    // ADJ(MULO(O1 O2)) -> MULO(ADJ(O2) ADJ(O1))
    DIRACOQ_RULE_DEF(R_ADJO3, kernel, term);

    // TSR(SCR(a X1) X2) -> SCR(a TSR(X1 X2))
    DIRACOQ_RULE_DEF(R_TSR0, kernel, term);

    // TSR(X1 SCR(a X2)) -> SCR(a TSR(X1 X2))
    DIRACOQ_RULE_DEF(R_TSR1, kernel, term);

    // TSR(ADD(X1 ... Xn) Y) -> ADD(TSR(X1 Y) ... TSR(Xn Y))
    DIRACOQ_RULE_DEF(R_TSR2, kernel, term);

    // TSR(Y ADD(X1 ... Xn)) -> ADD(TSR(Y X1) ... TSR(Y Xn))
    DIRACOQ_RULE_DEF(R_TSR3, kernel, term);

    // K : KType(T2) => TSR(0K(T1) K) -> 0K(Prod(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRK0, kernel, term);

    // K : KType(T1) => TSR(K 0K(T2)) -> 0K(Prod(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRK1, kernel, term);

    // TSR(KET(s) KET(t)) -> KET(PAIR(s t))
    DIRACOQ_RULE_DEF(R_TSRK2, kernel, term);

    // B : BType(T2) => TSR(0B(T1) B) -> 0B(Prod(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRB0, kernel, term);

    // B : BType(T1) => TSR(B 0B(T2)) -> 0B(Prod(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRB1, kernel, term);

    // TSR(BRA(s) BRA(t)) -> BRA(PAIR(s t))
    DIRACOQ_RULE_DEF(R_TSRB2, kernel, term);

    // O : OType(T3 T4) => TSR(0O(T1 T2) O) -> 0O(Prod(T1 T3) Prod(T2 T4))
    DIRACOQ_RULE_DEF(R_TSRO0, kernel, term);

    // O : OType(T1 T2) => TSR(O 0O(T3 T4)) -> 0O(Prod(T1 T3) Prod(T2 T4))
    DIRACOQ_RULE_DEF(R_TSRO1, kernel, term);

    // TSR(1O(T1) 1O(T2)) -> 1O(Prod(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRO2, kernel, term);

    // TSR(OUTER(K1 B1) OUTER(K2 B2)) -> OUTER(TSR(K1 K2) TSR(B1 B2))
    DIRACOQ_RULE_DEF(R_TSRO3, kernel, term);

    // MULK(0O(T1 T2) K) -> 0K(T1)
    DIRACOQ_RULE_DEF(R_MULK0, kernel, term);

    // O : OType(T1 T2) => MULK(O 0K(T2)) -> 0K(T1)
    DIRACOQ_RULE_DEF(R_MULK1, kernel, term);

    // MULK(1O(T) K) -> K
    DIRACOQ_RULE_DEF(R_MULK2, kernel, term);

    // MULK(SCR(a O) K) -> SCR(a MULK(O K))
    DIRACOQ_RULE_DEF(R_MULK3, kernel, term);

    // MULK(O SCR(a K)) -> SCR(a MULK(O K))
    DIRACOQ_RULE_DEF(R_MULK4, kernel, term);

    // MULK(ADD(O1 ... On) K) -> ADD(MULK(O1 K) ... MULK(On K))
    DIRACOQ_RULE_DEF(R_MULK5, kernel, term);

    // MULK(O ADD(K1 ... Kn)) -> ADD(MULK(O K1) ... MULK(O Kn))
    DIRACOQ_RULE_DEF(R_MULK6, kernel, term);

    // MULK(OUTER(K1 B) K2) -> SCR(DOT(B K2) K1)
    DIRACOQ_RULE_DEF(R_MULK7, kernel, term);

    // MULK(MULO(O1 O2) K) -> MULK(O1 MULK(O2 K))
    DIRACOQ_RULE_DEF(R_MULK8, kernel, term);

    // MULK(TSR(O1 O2) MULK(TSR(O3 O4) K)) -> MULK(TSR(MULO(O1 O3) MULO(O2 O4)) K)
    DIRACOQ_RULE_DEF(R_MULK9, kernel, term);

    // MULK(TSRO(O1 O2) KET(PAIR(s t))) -> TSR(MULK(O1 KET(s)) MULK(O2 KET(t)))
    DIRACOQ_RULE_DEF(R_MULK10, kernel, term);

    // MULK(TSR(O1 O2) TSR(K1 K2)) -> TSR(MULK(O1 K1) MULK(O2 K2))
    DIRACOQ_RULE_DEF(R_MULK11, kernel, term);

    // MULB(B 0O(T1 T2)) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_MULB0, kernel, term);

    // O : OType(T1 T2) => MULB(0B(T1) O) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_MULB1, kernel, term);

    // MULB(B 1O(T)) -> B
    DIRACOQ_RULE_DEF(R_MULB2, kernel, term);

    // MULB(SCR(a B) O) -> SCR(a MULB(B O))
    DIRACOQ_RULE_DEF(R_MULB3, kernel, term);

    // MULB(B SCR(a O)) -> SCR(a MULB(B O))
    DIRACOQ_RULE_DEF(R_MULB4, kernel, term);

    // MULB(ADD(B1 ... Bn) O) -> ADD(MULB(B1 O) ... MULB(Bn O))
    DIRACOQ_RULE_DEF(R_MULB5, kernel, term);

    // MULB(B ADD(O1 ... On)) -> ADD(MULB(B O1) ... MULB(B On))
    DIRACOQ_RULE_DEF(R_MULB6, kernel, term);

    // MULB(B1 OUTER(K B2)) -> SCR(DOT(B1 K) B2)
    DIRACOQ_RULE_DEF(R_MULB7, kernel, term);

    // MULB(B MULO(O1 O2)) -> MULB(MULB(B O1) O2)
    DIRACOQ_RULE_DEF(R_MULB8, kernel, term);

    // MULB(MULB(B TSR(O1 O2)) TSR(O3 O4)) -> MULB(B TSR(MULO(O1 O3) MULO(O2 O4)))
    DIRACOQ_RULE_DEF(R_MULB9, kernel, term);

    // MULB(BRA(PAIR(s t)) TSR(O1 O2)) -> TSR(MULB(BRA(s) O1) MULB(BRA(t) O2))
    DIRACOQ_RULE_DEF(R_MULB10, kernel, term);

    // MULB(TSR(B1 B2) TSR(O1 O2)) -> TSR(MULB(B1 O1) MULB(B2 O2))
    DIRACOQ_RULE_DEF(R_MULB11, kernel, term);

    // B : B(T2) => OUTER(0K(T1) B) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_OUTER0, kernel, term);

    // K : K(T1) => OUTER(K 0B(T2)) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_OUTER1, kernel, term);

    // OUTER(SCR(a K) B) -> SCR(a OUTER(K B))
    DIRACOQ_RULE_DEF(R_OUTER2, kernel, term);

    // OUTER(K SCR(a B)) -> SCR(a OUTER(K B))
    DIRACOQ_RULE_DEF(R_OUTER3, kernel, term);

    // OUTER(ADD(K1 ... Kn) B) -> ADD(OUTER(K1 B) ... OUTER(Kn B))
    DIRACOQ_RULE_DEF(R_OUTER4, kernel, term);

    // OUTER(K ADD(B1 ... Bn)) -> ADD(OUTER(K B1) ... OUTER(K Bn))
    DIRACOQ_RULE_DEF(R_OUTER5, kernel, term);

    // O : OType(T2 T3) => MULO(0O(T1 T2) O) -> 0O(T1 T3)
    DIRACOQ_RULE_DEF(R_MULO0, kernel, term);

    // O : OType(T1 T2) => MULO(O 0O(T2 T3)) -> 0O(T1 T3)
    DIRACOQ_RULE_DEF(R_MULO1, kernel, term);

    // MULO(1O(T) O) -> O
    DIRACOQ_RULE_DEF(R_MULO2, kernel, term);

    // MULO(O 1O(T)) -> O
    DIRACOQ_RULE_DEF(R_MULO3, kernel, term);

    // MULO(OUTER(K B) O) -> OUTER(K MULB(B O))
    DIRACOQ_RULE_DEF(R_MULO4, kernel, term);

    // MULO(O OUTER(K B)) -> OUTER(MULK(O K) B)
    DIRACOQ_RULE_DEF(R_MULO5, kernel, term);

    // MULO(SCR(a O1) O2) -> SCR(a MULO(O1 O2))
    DIRACOQ_RULE_DEF(R_MULO6, kernel, term);

    // MULO(O1 SCR(a O2)) -> SCR(a MULO(O1 O2))
    DIRACOQ_RULE_DEF(R_MULO7, kernel, term);

    // MULO(ADD(O1 ... On) O) -> ADD(MULO(O1 O) ... MULO(On O))
    DIRACOQ_RULE_DEF(R_MULO8, kernel, term);

    // MULO(O ADD(O1 ... On)) -> ADD(MULO(O O1) ... MULO(O On))
    DIRACOQ_RULE_DEF(R_MULO9, kernel, term);

    // MULO(MULO(O1 O2) O3) -> MULO(O1 MULO(O2 O3))
    DIRACOQ_RULE_DEF(R_MULO10, kernel, term);

    // MULO(TSR(O1 O2) TSR(O3 O4)) -> TSR(MULO(O1 O3) MULO(O2 O4))
    DIRACOQ_RULE_DEF(R_MULO11, kernel, term);

    // MULO(TSR(O1 O2) MULO(TSR(O3 O4) O)) -> MULO(TSR(MULO(O1 O3) MULO(O2 O4)) O)
    DIRACOQ_RULE_DEF(R_MULO12, kernel, term);

    // CATPROD(USET(T1) USET(T2)) -> USET(Prod(T1 T2))
    DIRACOQ_RULE_DEF(R_SET0, kernel, term);

    // SUM(s fun(x T 0)) -> 0
    DIRACOQ_RULE_DEF(R_SUM_CONST0, kernel, term);

    // SUM(s fun(x T1 0K(T2))) -> 0K(T2)
    DIRACOQ_RULE_DEF(R_SUM_CONST1, kernel, term);

    // SUM(s fun(x T1 0B(T2))) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_SUM_CONST2, kernel, term);

    // SUM(s fun(x T1 0O(T2 T3))) -> 0O(T2 T3)
    DIRACOQ_RULE_DEF(R_SUM_CONST3, kernel, term);

    // 1O(T) -> SUM(USET(T) fun(i T OUTER(KET(i) BRA(i))))
    DIRACOQ_RULE_DEF(R_SUM_CONST4, kernel, term);

    // i free in t => SUM(USET(T) fun(i T SUM(... DELTA(i t) ...))) -> SUM(... 1 ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM0, kernel, term); 

    // i free in t => SUM(USET(T) fun(i T SUM(... MULS(a1 ... DELTA(i t) ... an) ...))) -> SUM(... MULS(a1{i/t} ... an{i/t}) ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM1, kernel, term); 

    // i free in t => SUM(USET(T) fun(i T SUM(... SCR(DELTA(i t) A) ...))) -> SUM(... A{i/t} ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM2, kernel, term); 

    // i free in t => SUM(USET(T) fun(i T SUM(... SCR(MULS(a1 ... DELTA(i t) ... an) A) ...))) -> SUM(... SCR(MULS(a1{i/t} ... an{i/t}) A{i/t}) ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM3, kernel, term); 

    // SUM(M fun(i T SUM(M fun(j T SUM(... DELTA(i j) ...))))) -> SUM(M fun(j T SUM(... 1 ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM4, kernel, term);

    // SUM(M fun(i T SUM(M fun(j T SUM(... MULS(a1 ... DELTA(i j) ... an) ...))))) -> SUM(M fun(j T SUM(... MULS(a1{j/i} ... an{j/i}) ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM5, kernel, term);

    // SUM(M fun(i T SUM(M fun(j T SUM(... SCR(DELTA(i j) A) ...))))) -> SUM(M fun(j T SUM(... A{j/i} ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM6, kernel, term);

    // SUM(M fun(i T SUM(M fun(j T SUM(... SCR(MULS(a1 ... DELTA(i j) ... an) A) ...))))) -> SUM(M fun(j T SUM(... SCR(MULS(a1{j/i} ... an{j/i}) A{j/i}) ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM7, kernel, term);

    // MULS(b1 ... SUM(M fun(i T a)) ... bn) -> SUM(M fun(i T MULS(b1 ... a ... bn)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH0, kernel, term);

    // CONJ(SUM(M fun(i T a))) -> SUM(M fun(i T CONJ(a)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH1, kernel, term);

    // ADJ(SUM(M fun(i T X))) -> SUM(M fun(i T ADJ(X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH2, kernel, term);

    // SCR(a SUM(M fun(i T X))) -> SUM(M fun(i T SCR(a X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH3, kernel, term);

    // SCR(SUM(M fun(i T a)) X) -> SUM(M fun(i T SCR(a X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH4, kernel, term);

    // DOT(SUM(M fun(i T B)) K) -> SUM(M fun(i T DOT(B K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH5, kernel, term);

    // MULK(SUM(M fun(i T O)) K) -> SUM(M fun(i T MULK(O K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH6, kernel, term);

    // MULB(SUM(M fun(i T B)) O) -> SUM(M fun(i T MULB(B O)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH7, kernel, term);
    
    // OUTER(SUM(M fun(i T K)) B) -> SUM(M fun(i T OUTER(K B)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH8, kernel, term);
    
    // MULO(SUM(M fun(i T O1)) O2) -> SUM(M fun(i T MULO(O1 O2)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH9, kernel, term);

    // DOT(B SUM(M fun(i T K))) -> SUM(M fun(i T DOT(B K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH10, kernel, term);

    // MULK(O SUM(M fun(i T K))) -> SUM(M fun(i T MULK(O K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH11, kernel, term);

    // MULB(B SUM(M fun(i T O))) -> SUM(M fun(i T MULB(B O)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH12, kernel, term);

    // OUTER(K SUM(M fun(i T B))) -> SUM(M fun(i T OUTER(K B)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH13, kernel, term);

    // MULO(O1 SUM(M fun(i T O2)) -> SUM(M fun(i T MULO(O1 O2)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH14, kernel, term);

    // TSR(SUM(M fun(i T X)) Y) -> SUM(M fun(i T TSR(X Y)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH15, kernel, term);

    // TSR(X SUM(M fun(i T Y))) -> SUM(M fun(i T TSR(X Y)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH16, kernel, term);

    // SUM(M fun(i T ADDS(a1 ... an))) -> ADDS(SUM(M fun(i T a1)) ... SUM(M fun(i T an)))
    DIRACOQ_RULE_DEF(R_SUM_ADDS0, kernel, term);

    // SUM(M fun(i T ADD(a1 ... an))) -> ADD(SUM(M fun(i T a1)) ... SUM(M fun(i T an)))
    DIRACOQ_RULE_DEF(R_SUM_ADD0, kernel, term);

    // R-SUM-ADD1 ~ R-SUM-ADD3 are omitted

    // SUM(USET(Prod(T1 T2)) fun(i Prod(T1 T2) X)) -> SUM(USET(T1) fun(j T1 SUM(USET(T2) fun(k T2 X{i/PAIR(j k)}))))
    DIRACOQ_RULE_DEF(R_SUM_INDEX0, kernel, term);

    // SUM(CATPROD(M1 M2) fun(i Prod(T1 T2) X)) -> SUM(M1 fun(j T1 SUM(M2 fun(k T2 X{i/PAIR(j k)})))
    DIRACOQ_RULE_DEF(R_SUM_INDEX1, kernel, term);
    

    // M1 < M2 => SUM(M2 fun(i T1 SUM(M1 fun(j T2 X))) -> SUM(M1 fun(j T2 SUM(M2 fun(i T1 X))))
    DIRACOQ_RULE_DEF(R_SUM_SWAP, kernel, term);


    // The scalar rule list.
    extern const std::vector<PosRewritingRule> rules;

    ///////////////// Trace Output

} // namespace scalar_vec
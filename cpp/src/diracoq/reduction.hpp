#pragma once

#include "ualg.hpp"
#include "WSTPinterface.hpp"

namespace diracoq {

    // The rewriting rules of the Diracoq kernel.
    using PosRewritingRule = std::optional<ualg::TermPtr<int>> (*)(Kernel& kernel, ualg::TermPtr<int> term);

#define DIRACOQ_RULE_DEF(name, kernel, term) std::optional<ualg::TermPtr<int>> name(diracoq::Kernel& kernel, ualg::TermPtr<int> term)

    // The struct for the rewriting trace. It records which rule is applied, at which position, and the new replacement.
    struct PosReplaceRecord {
        ualg::TermPtr<int> init_term;
        PosRewritingRule rule;
        ualg::TermPos pos;
        ualg::TermPtr<int> replacement;
    };


    /**
     * @brief Get the rewriting record using the rewriting rules.
     * 
     * @param kernel 
     * @param term 
     * @param rules 
     * @return std::optional<PosReplaceRecord> 
     */
    std::optional<PosReplaceRecord> get_pos_replace(Kernel& kernel, ualg::TermPtr<int> term, const std::vector<PosRewritingRule>& rules);


    /**
     * @brief Rewrite the term repeatedly using the given rewriting rules, until no more rules can apply.
     * 
     * @param kernel 
     * @param term 
     * @param rules 
     * @param trace The container to store the trace of the rewriting.
     * @return const NormalTerm<int>* 
     */
    ualg::TermPtr<int> pos_rewrite_repeated(Kernel& kernel, ualg::TermPtr<int> term, const std::vector<PosRewritingRule>& rules, 
    std::vector<PosReplaceRecord>* trace = nullptr);

    /**
     * @brief This function rename all the bound variables in the term and return the result.
     * 
     * @param kernel 
     * @param term 
     * @param bound_vars 
     * @return ualg::TermPtr<int> 
     */
    ualg::TermPtr<int> bound_variable_rename(Kernel& kernel, ualg::TermPtr<int> term);

    /**
     * @brief Expand all the variables in the term once.
     * 
     * @param kernel 
     * @param term 
     * @return ualg::TermPtr<int> 
     */
    ualg::TermPtr<int> variable_expand(Kernel& kernel, ualg::TermPtr<int> term);

    /**
     * @brief Iterate through the whole term and rename the bound variables.
     * 
     * This function should be used in the end of the rewriting process, on the whole term, to ensure that the bound variables are correctly renamed.
     * 
     * @param kernel 
     * @param term 
     * @return const ualg::NormalTerm<int>* 
     */
    inline ualg::TermPtr<int> deBruijn_normalize(Kernel& kernel, ualg::TermPtr<int> term){
        return to_deBruijn(kernel.get_sig(), term);
    }

    
    /**
     * @brief Get the bound vars of the term (in the FUN expression)
     * 
     * @param term 
     * @return std::set<int> 
     */
    std::set<int> get_bound_vars(ualg::TermPtr<int> term);


    /**
     * @brief Compare the terms modulo the bound variables.
     * 
     * @param termA 
     * @param termB 
     * @param bound_vars
     * @return true termA < termB
     * @return false termA >= termB
     */
    bool comp_modulo_bound_vars(ualg::TermPtr<int> termA, ualg::TermPtr<int> termB, const std::set<int>& bound_vars);


    /**
     * @brief Transform a sorted term modulo bound variables to a normal term under sum_swap.
     * 
     * @param kernel 
     * @param term 
     * @return ualg::TermPtr<int> 
     */
    ualg::TermPtr<int> sum_swap_normalization(Kernel& kernel, ualg::TermPtr<int> term);

    /**
     * @brief Use the Wolfram engine to simplify the term.
     * 
     * @param kernel 
     * @param term 
     * @return ualg::TermPtr<int> 
     */
    ualg::TermPtr<int> wolfram_fullsimplify(Kernel& kernel, ualg::TermPtr<int> term);



    //////////////// Rules

    // a : STYPE, b : STYPE => a @ b -> MULS(a b)
    DIRACOQ_RULE_DEF(R_COMPO_SS, kernel, term);

    // a : STYPE, K : KTYPE(T) => a @ K -> SCR(a K)
    DIRACOQ_RULE_DEF(R_COMPO_SK, kernel, term);

    // a : STYPE, B : BTYPE(T) => a @ B -> SCR(a B)
    DIRACOQ_RULE_DEF(R_COMPO_SB, kernel, term);

    // a : STYPE, O : OTYPE(T1 T2) => a @ O -> SCR(a O)
    DIRACOQ_RULE_DEF(R_COMPO_SO, kernel, term);

    // K : KTYPE(T), a : STYPE => COMPO(K a) -> SCR(a K)
    DIRACOQ_RULE_DEF(R_COMPO_KS, kernel, term);

    // K1 : KTYPE(T1), K2 : KTYPE(T2) => COMPO(K1 K2) -> TSR(K1 K2)
    DIRACOQ_RULE_DEF(R_COMPO_KK, kernel, term);

    // K : KTYPE(T1), B : BTYPE(T2) => COMPO(K B) -> DOT(K B)
    DIRACOQ_RULE_DEF(R_COMPO_KB, kernel, term);

    // B : BTYPE(T), a : STYPE => COMPO(B a) -> SCR(a B)
    DIRACOQ_RULE_DEF(R_COMPO_BS, kernel, term);

    // B : BTYPE(T), K : KTYPE(T) => COMPO(B K) -> DOT(B K)
    DIRACOQ_RULE_DEF(R_COMPO_BK, kernel, term);

    // B1 : BTYPE(T1), B2 : BTYPE(T2) => COMPO(B1 B2) -> TSR(B1 B2)
    DIRACOQ_RULE_DEF(R_COMPO_BB, kernel, term);

    // B : BTYPE(T1), O : OTYPE(T1 T2) => COMPO(B O) -> DOT(B O)
    DIRACOQ_RULE_DEF(R_COMPO_BO, kernel, term);

    // O : OTYPE(T1 T2), a : STYPE => COMPO(O a) -> SCR(a O)
    DIRACOQ_RULE_DEF(R_COMPO_OS, kernel, term);

    // O : OTYPE(T1 T2), K : KTYPE(T2) => COMPO(O K) -> DOT(O K)
    DIRACOQ_RULE_DEF(R_COMPO_OK, kernel, term);

    // O1 : OTYPE(T1 T2), O2 : OTYPE(T2 T3) => COMPO(O1 O2) -> DOT(O1 O2)
    DIRACOQ_RULE_DEF(R_COMPO_OO, kernel, term);

    // f : T1 -> T2 => COMPO(f a) -> APPLY(f a)
    DIRACOQ_RULE_DEF(R_COMPO_ARROW, kernel, term);

    // f : FORALL(x T1) => COMPO(f a) -> APPLY(f a)
    DIRACOQ_RULE_DEF(R_COMPO_FORALL, kernel, term);

    // T1 : INDEX, T2 : INDEX => STAR(T1 T2) -> PROD(T1 T2)
    DIRACOQ_RULE_DEF(R_STAR_PROD, kernel, term);

    // an : SType => STAR(a1 ... an) -> MULS(a1 ... an)
    DIRACOQ_RULE_DEF(R_STAR_MULS, kernel, term);

    // O1 : OType(T1 T2), O2 : OType(T3 T3) => STAR(O1 O2) -> TSR(O1 O2)
    DIRACOQ_RULE_DEF(R_STAR_TSRO, kernel, term);

    // S1 : SET(T1), S2 : SET(T2) => STAR(S1 S2) -> CATPROD(S1 S2)
    DIRACOQ_RULE_DEF(R_STAR_CATPROD, kernel, term);

    // an : STYPE => ADDG(a1 ... an) -> ADDS(a1 ... an)
    DIRACOQ_RULE_DEF(R_ADDG_ADDS, kernel, term);

    // an : KTYPE(T), BTYPE(T) or OTYPE(T1 T2) => ADDG(a1 ... an) -> ADD(a1 ... an)
    DIRACOQ_RULE_DEF(R_ADDG_ADD, kernel, term);

    // S : SET(T) => SSUM(i S e) -> SUM(S FUN(i BASIS(T) e))
    DIRACOQ_RULE_DEF(R_SSUM, kernel, term);


    // The rules for pre-processing
    extern const std::vector<PosRewritingRule> pre_proc_rules;


    // beta reduction
    DIRACOQ_RULE_DEF(R_BETA_ARROW, kernel, term);
    DIRACOQ_RULE_DEF(R_BETA_INDEX, kernel, term);

    // delta reduction
    DIRACOQ_RULE_DEF(R_DELTA, kernel, term);

    // //////////////// Flattening AC symbols
    DIRACOQ_RULE_DEF(R_FLATTEN, kernel, term);


    // //////////////// properties of the symbols

    // ADDS(a) -> a
    DIRACOQ_RULE_DEF(R_ADDSID, kernel, term);
    // MULS(a) -> a
    DIRACOQ_RULE_DEF(R_MULSID, kernel, term);


    // //////////////// rewriting rules

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

    // K : KTYPE(T) => SCR(0 K) -> 0K(T)
    DIRACOQ_RULE_DEF(R_SCRK0, kernel, term);

    // SCR(a 0K(T)) -> 0K(T)
    DIRACOQ_RULE_DEF(R_SCRK1, kernel, term);

    // B : BTYPE(T) => SCR(0 B) -> 0B(T)
    DIRACOQ_RULE_DEF(R_SCRB0, kernel, term);

    // SCR(a 0B(T)) -> 0B(T)
    DIRACOQ_RULE_DEF(R_SCRB1, kernel, term);

    // O : OTYPE(T1 T2) => SCR(0 O) -> 0O(T1 T2)
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

    // ADJ(DOT(B O)) -> DOT(ADJ(O) ADJ(B))
    DIRACOQ_RULE_DEF(R_ADJK2, kernel, term);

    // ADJ(0K(T)) -> 0B(T)
    DIRACOQ_RULE_DEF(R_ADJB0, kernel, term);

    // ADJ(KET(t)) -> BRA(t)
    DIRACOQ_RULE_DEF(R_ADJB1, kernel, term);

    // ADJ(DOT(O K)) -> DOT(ADJ(K) ADJ(O))
    DIRACOQ_RULE_DEF(R_ADJB2, kernel, term);

    // ADJ(0O(T1 T2)) -> 0O(T2 T1)
    DIRACOQ_RULE_DEF(R_ADJO0, kernel, term);

    // ADJ(1O(T)) -> 1O(T)
    DIRACOQ_RULE_DEF(R_ADJO1, kernel, term);

    // ADJ(DOT(K B)) -> DOT(ADJ(B) ADJ(K))
    DIRACOQ_RULE_DEF(R_ADJO2, kernel, term);

    // ADJ(DOT(O1 O2)) -> DOT(ADJ(O2) ADJ(O1))
    DIRACOQ_RULE_DEF(R_ADJO3, kernel, term);

    // TSR(SCR(a X1) X2) -> SCR(a TSR(X1 X2))
    DIRACOQ_RULE_DEF(R_TSR0, kernel, term);

    // TSR(X1 SCR(a X2)) -> SCR(a TSR(X1 X2))
    DIRACOQ_RULE_DEF(R_TSR1, kernel, term);

    // TSR(ADD(X1 ... Xn) Y) -> ADD(TSR(X1 Y) ... TSR(Xn Y))
    DIRACOQ_RULE_DEF(R_TSR2, kernel, term);

    // TSR(Y ADD(X1 ... Xn)) -> ADD(TSR(Y X1) ... TSR(Y Xn))
    DIRACOQ_RULE_DEF(R_TSR3, kernel, term);

    // K : KTYPE(T2) => TSR(0K(T1) K) -> 0K(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRK0, kernel, term);

    // K : KTYPE(T1) => TSR(K 0K(T2)) -> 0K(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRK1, kernel, term);

    // TSR(KET(s) KET(t)) -> KET(PAIR(s t))
    DIRACOQ_RULE_DEF(R_TSRK2, kernel, term);

    // B : BTYPE(T2) => TSR(0B(T1) B) -> 0B(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRB0, kernel, term);

    // B : BTYPE(T1) => TSR(B 0B(T2)) -> 0B(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRB1, kernel, term);

    // TSR(BRA(s) BRA(t)) -> BRA(PAIR(s t))
    DIRACOQ_RULE_DEF(R_TSRB2, kernel, term);

    // O : OTYPE(T3 T4) => TSR(0O(T1 T2) O) -> 0O(PROD(T1 T3) PROD(T2 T4))
    DIRACOQ_RULE_DEF(R_TSRO0, kernel, term);

    // O : OTYPE(T1 T2) => TSR(O 0O(T3 T4)) -> 0O(PROD(T1 T3) PROD(T2 T4))
    DIRACOQ_RULE_DEF(R_TSRO1, kernel, term);

    // TSR(1O(T1) 1O(T2)) -> 1O(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_TSRO2, kernel, term);

    // TSR(DOT(K1 B1) DOT(K2 B2)) -> DOT(TSR(K1 K2) TSR(B1 B2))
    DIRACOQ_RULE_DEF(R_TSRO3, kernel, term);

    ///////////////////////// DOT

    // DOT(1O(T) O) -> O
    DIRACOQ_RULE_DEF(R_MULO2, kernel, term);

    // DOT(O 1O(T)) -> O
    DIRACOQ_RULE_DEF(R_MULO3, kernel, term);

    // DOT(SCR(a X) Y) -> SCR(a DOT(X Y))
    DIRACOQ_RULE_DEF(R_MULK3, kernel, term);

    // DOT(X SCR(a Y)) -> SCR(a DOT(X Y))
    DIRACOQ_RULE_DEF(R_MULK4, kernel, term);

    // DOT(ADD(B1 ... Bn) K) -> ADDS(DOT(B1 K) ... DOT(Bn K))
    DIRACOQ_RULE_DEF(R_MULK5, kernel, term);

    // DOT(B ADD(K1 ... Kn)) -> ADDS(DOT(B K1) ... DOT(B Kn))
    DIRACOQ_RULE_DEF(R_MULK6, kernel, term);

    // DOT(TSR(B1 B2) DOT(TSR(O1 O2) K)) -> DOT(TSR(DOT(B1 O1) DOT(B2 O2)) K)
    DIRACOQ_RULE_DEF(R_DOT12, kernel, term);


    // DOT(0B(sigma) K) -> 0
    DIRACOQ_RULE_DEF(R_DOT0, kernel, term);

    // DOT(B 0K(sigma)) -> 0
    DIRACOQ_RULE_DEF(R_DOT1, kernel, term);

    // DOT(SCR(a B) K) -> MULS(a DOT(B K))
    DIRACOQ_RULE_DEF(R_DOT2, kernel, term);

    // DOT(B SCR(a K)) -> MULS(a DOT(B K))
    DIRACOQ_RULE_DEF(R_DOT3, kernel, term);

    // DOT(ADD(B1 ... Bn) K) -> ADDS(DOT(B1 K) ... DOT(Bn K))
    DIRACOQ_RULE_DEF(R_DOT4, kernel, term);

    // DOT(B ADD(K1 ... Kn)) -> ADDS(DOT(B K1) ... DOT(B Kn))
    DIRACOQ_RULE_DEF(R_DOT5, kernel, term);

    // DOT(BRA(s) KET(t)) -> DELTA(s t)
    DIRACOQ_RULE_DEF(R_DOT6, kernel, term);

    // DOT(TSR(B1 B2) KET(PAIR(s t))) -> MULS(DOT(B1 KET(s)) DOT(B2 KET(t)))
    DIRACOQ_RULE_DEF(R_DOT7, kernel, term);

    // DOT(BRA(PAIR(s t)) TSR(K1 K2)) -> MULS(DOT(BRA(s) K1) DOT(BRA(t) K2))
    DIRACOQ_RULE_DEF(R_DOT8, kernel, term);

    // DOT(TSR(B1 B2) TSR(K1 K2)) -> MULS(DOT(B1 K1) DOT(B2 K2))
    DIRACOQ_RULE_DEF(R_DOT9, kernel, term);

    // DOT(DOT(B O) K) -> DOT(B DOT(O K))
    DIRACOQ_RULE_DEF(R_DOT10, kernel, term);

    // DOT(BRA(PAIR(s t)) DOT(TSR(O1 O2) K)) -> DOT(TSR(DOT(BRA(s) O1) DOT(BRA(t) O2)) K)
    DIRACOQ_RULE_DEF(R_DOT11, kernel, term);

    // DOT(0O(T1 T2) K) -> 0K(T1)
    DIRACOQ_RULE_DEF(R_MULK0, kernel, term);

    // O : OTYPE(T1 T2) => DOT(O 0K(T2)) -> 0K(T1)
    DIRACOQ_RULE_DEF(R_MULK1, kernel, term);

    // DOT(DOT(K1 B) K2) -> SCR(DOT(B K2) K1)
    DIRACOQ_RULE_DEF(R_MULK7, kernel, term);

    // DOT(DOT(O1 O2) K) -> DOT(O1 DOT(O2 K))
    DIRACOQ_RULE_DEF(R_MULK8, kernel, term);

    // DOT(TSRO(O1 O2) KET(PAIR(s t))) -> TSR(DOT(O1 KET(s)) DOT(O2 KET(t)))
    DIRACOQ_RULE_DEF(R_MULK10, kernel, term);

    // DOT(TSR(O1 O2) TSR(K1 K2)) -> TSR(DOT(O1 K1) DOT(O2 K2))
    DIRACOQ_RULE_DEF(R_MULK11, kernel, term);

    // DOT(B 0O(T1 T2)) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_MULB0, kernel, term);

    // O : OTYPE(T1 T2) => DOT(0B(T1) O) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_MULB1, kernel, term);

    // DOT(B1 DOT(K B2)) -> SCR(DOT(B1 K) B2)
    DIRACOQ_RULE_DEF(R_MULB7, kernel, term);

    // DOT(B DOT(O1 O2)) -> DOT(DOT(B O1) O2)
    DIRACOQ_RULE_DEF(R_MULB8, kernel, term);

    // DOT(DOT(B TSR(O1 O2)) TSR(O3 O4)) -> DOT(B TSR(DOT(O1 O3) DOT(O2 O4)))
    DIRACOQ_RULE_DEF(R_MULB9, kernel, term);

    // DOT(BRA(PAIR(s t)) TSR(O1 O2)) -> TSR(DOT(BRA(s) O1) DOT(BRA(t) O2))
    DIRACOQ_RULE_DEF(R_MULB10, kernel, term);

    // DOT(TSR(B1 B2) TSR(O1 O2)) -> TSR(DOT(B1 O1) DOT(B2 O2))
    DIRACOQ_RULE_DEF(R_MULB11, kernel, term);

    // B : B(T2) => DOT(0K(T1) B) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_OUTER0, kernel, term);

    // K : K(T1) => DOT(K 0B(T2)) -> 0O(T1 T2)
    DIRACOQ_RULE_DEF(R_OUTER1, kernel, term);

    // O : OTYPE(T2 T3) => DOT(0O(T1 T2) O) -> 0O(T1 T3)
    DIRACOQ_RULE_DEF(R_MULO0, kernel, term);

    // O : OTYPE(T1 T2) => DOT(O 0O(T2 T3)) -> 0O(T1 T3)
    DIRACOQ_RULE_DEF(R_MULO1, kernel, term);

    // DOT(DOT(K B) O) -> DOT(K DOT(B O))
    DIRACOQ_RULE_DEF(R_MULO4, kernel, term);

    // DOT(O DOT(K B)) -> DOT(DOT(O K) B)
    DIRACOQ_RULE_DEF(R_MULO5, kernel, term);

    // DOT(DOT(O1 O2) O3) -> DOT(O1 DOT(O2 O3))
    DIRACOQ_RULE_DEF(R_MULO10, kernel, term);

    // DOT(TSR(O1 O2) TSR(O3 O4)) -> TSR(DOT(O1 O3) DOT(O2 O4))
    DIRACOQ_RULE_DEF(R_MULO11, kernel, term);


    /////////// Big Operator

    // CATPROD(USET(T1) USET(T2)) -> USET(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_SET0, kernel, term);

    // CATPROD(USET(T1) USET(T2)) -> USET(PROD(T1 T2))
    DIRACOQ_RULE_DEF(R_SET0, kernel, term);

    // SUM(s FUN(x T 0)) -> 0
    DIRACOQ_RULE_DEF(R_SUM_CONST0, kernel, term);

    // SUM(s FUN(x T1 0K(T2))) -> 0K(T2)
    DIRACOQ_RULE_DEF(R_SUM_CONST1, kernel, term);

    // SUM(s FUN(x T1 0B(T2))) -> 0B(T2)
    DIRACOQ_RULE_DEF(R_SUM_CONST2, kernel, term);

    // SUM(s FUN(x T1 0O(T2 T3))) -> 0O(T2 T3)
    DIRACOQ_RULE_DEF(R_SUM_CONST3, kernel, term);

    // 1O(T) -> SUM(USET(T) FUN(i T DOT(KET(i) BRA(i))))
    DIRACOQ_RULE_DEF(R_SUM_CONST4, kernel, term);

    // i free in t => SUM(USET(T) FUN(i T SUM(... DELTA(i t) ...))) -> SUM(... 1 ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM0, kernel, term); 

    // i free in t => SUM(USET(T) FUN(i T SUM(... MULS(a1 ... DELTA(i t) ... an) ...))) -> SUM(... MULS(a1{i/t} ... an{i/t}) ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM1, kernel, term); 

    // i free in t => SUM(USET(T) FUN(i T SUM(... SCR(DELTA(i t) A) ...))) -> SUM(... A{i/t} ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM2, kernel, term); 

    // i free in t => SUM(USET(T) FUN(i T SUM(... SCR(MULS(a1 ... DELTA(i t) ... an) A) ...))) -> SUM(... SCR(MULS(a1{i/t} ... an{i/t}) A{i/t}) ...)
    DIRACOQ_RULE_DEF(R_SUM_ELIM3, kernel, term); 

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... DELTA(i j) ...))))) -> SUM(M FUN(j T SUM(... 1 ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM4, kernel, term);

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... MULS(a1 ... DELTA(i j) ... an) ...))))) -> SUM(M FUN(j T SUM(... MULS(a1{j/i} ... an{j/i}) ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM5, kernel, term);

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... SCR(DELTA(i j) A) ...))))) -> SUM(M FUN(j T SUM(... A{j/i} ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM6, kernel, term);

    // SUM(M FUN(i T SUM(M FUN(j T SUM(... SCR(MULS(a1 ... DELTA(i j) ... an) A) ...))))) -> SUM(M FUN(j T SUM(... SCR(MULS(a1{j/i} ... an{j/i}) A{j/i}) ...)))
    DIRACOQ_RULE_DEF(R_SUM_ELIM7, kernel, term);

    // MULS(b1 ... SUM(M FUN(i T a)) ... bn) -> SUM(M FUN(i T MULS(b1 ... a ... bn)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH0, kernel, term);

    // CONJ(SUM(M FUN(i T a))) -> SUM(M FUN(i T CONJ(a)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH1, kernel, term);

    // ADJ(SUM(M FUN(i T X))) -> SUM(M FUN(i T ADJ(X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH2, kernel, term);

    // SCR(a SUM(M FUN(i T X))) -> SUM(M FUN(i T SCR(a X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH3, kernel, term);

    // SCR(SUM(M FUN(i T a)) X) -> SUM(M FUN(i T SCR(a X)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH4, kernel, term);

    // DOT(SUM(M FUN(i T B)) K) -> SUM(M FUN(i T DOT(B K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH5, kernel, term);

    // DOT(SUM(M FUN(i T O)) K) -> SUM(M FUN(i T DOT(O K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH6, kernel, term);

    // DOT(SUM(M FUN(i T B)) O) -> SUM(M FUN(i T DOT(B O)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH7, kernel, term);
    
    // DOT(SUM(M FUN(i T K)) B) -> SUM(M FUN(i T DOT(K B)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH8, kernel, term);
    
    // DOT(SUM(M FUN(i T O1)) O2) -> SUM(M FUN(i T DOT(O1 O2)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH9, kernel, term);

    // DOT(B SUM(M FUN(i T K))) -> SUM(M FUN(i T DOT(B K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH10, kernel, term);

    // DOT(O SUM(M FUN(i T K))) -> SUM(M FUN(i T DOT(O K)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH11, kernel, term);

    // DOT(B SUM(M FUN(i T O))) -> SUM(M FUN(i T DOT(B O)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH12, kernel, term);

    // DOT(K SUM(M FUN(i T B))) -> SUM(M FUN(i T DOT(K B)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH13, kernel, term);

    // DOT(O1 SUM(M FUN(i T O2)) -> SUM(M FUN(i T DOT(O1 O2)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH14, kernel, term);

    // TSR(SUM(M FUN(i T X)) Y) -> SUM(M FUN(i T TSR(X Y)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH15, kernel, term);

    // TSR(X SUM(M FUN(i T Y))) -> SUM(M FUN(i T TSR(X Y)))
    DIRACOQ_RULE_DEF(R_SUM_PUSH16, kernel, term);

    // SUM(M FUN(i T ADDS(a1 ... an))) -> ADDS(SUM(M FUN(i T a1)) ... SUM(M FUN(i T an)))
    DIRACOQ_RULE_DEF(R_SUM_ADDS0, kernel, term);

    // SUM(M FUN(i T ADD(a1 ... an))) -> ADD(SUM(M FUN(i T a1)) ... SUM(M FUN(i T an)))
    DIRACOQ_RULE_DEF(R_SUM_ADD0, kernel, term);

    // SUM(M FUN(i T SCR(ADDS(a1 ... an) X))) -> ADD(SUM(M FUN(i T SCR(a1 X))) ... SUM(M FUN(i T SCR(an X)))
    DIRACOQ_RULE_DEF(R_SUM_ADD1, kernel, term);

    // SUM(USET(PROD(T1 T2)) FUN(i BASIS(PROD(T1 T2)) X)) -> SUM(USET(T1) FUN(j BASIS(T1) SUM(USET(T2) FUN(k BASIS(T2) X{i/PAIR(j k)}))))
    DIRACOQ_RULE_DEF(R_SUM_INDEX0, kernel, term);

    // SUM(CATPROD(M1 M2) FUN(i BASIS(PROD(T1 T2)) X)) -> SUM(M1 FUN(j BASIS(T1) SUM(M2 FUN(k BASIS(T2) X{i/PAIR(j k)})))
    DIRACOQ_RULE_DEF(R_SUM_INDEX1, kernel, term);



    // DELTA(BASIS0 BASIS1) -> 0
    DIRACOQ_RULE_DEF(R_QBIT_DELTA, kernel, term);

    // ONEO(QBIT) -> ADD(DOT(KET(#0) BRA(#0)) DOT(KET(#1) BRA(#1))
    DIRACOQ_RULE_DEF(R_QBIT_ONEO, kernel, term);

    // SUM(USET(QBIT) FUN(i BASIS(QBIT) X)) -> ADD(X{i/#0} X{i/#1})
    DIRACOQ_RULE_DEF(R_QBIT_SUM, kernel, term);

    // The rule list.
    extern const std::vector<PosRewritingRule> rules;


    // The rule list when combined with wolfram engine. (scalar rules are not included)
    extern const std::vector<PosRewritingRule> rules_with_wolfram;

    ///////////////// Trace Output

} // namespace scalar_vec
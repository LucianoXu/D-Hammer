#include "examples.hpp"

namespace examples {

    std::vector<EqExample> eq_examples = {
        {
            "Example 1",
            R"(
                Var sigma : INDEX.
                Var K : KTYPE[sigma].
            )",
            "(TPK sigma) ((TPB sigma) K)",
            "K"
        },

/*
PRELIMINARY

Block[
 {DiracCtx = {M -> OType[T, T]}, phi},
 phi[T_] := 
  With[{nv = Unique[]}, Sum[Ket[{PAIR[nv, nv]}], {nv, USET[T]}]];
 DNEqQ[(M\[CircleTimes]ONEO[T])\[SmallCircle]phi[
    T], (ONEO[T]\[CircleTimes]TPO[M, T, T])\[SmallCircle]phi[T]]
 ]
 */

        {
            "PRELIMINARY",
            R"(
                Var T : INDEX.
                Var M : OTYPE[T, T].
                Def phi := idx T => Sum nv in USET[T], |(nv, nv)>.
            )",
            "(M * 1O[T]) (phi T)",
            "(1O[T] * (TPO T T M)) (phi T)"
        },
/*
QCQI-1

Block[
 {DiracCtx = {A -> OType[T1, T2], M -> SetType[m], a[_] -> SType, 
    v[_] -> KType[T2]}},
 DNEqQ[
  A\[SmallCircle]Sum[a[i] v[i], {i, M}],
  Sum[a[i] (A\[SmallCircle]v[i]), {i, M}]
  ]
 ]
*/

        {
            "QCQI-1",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var m : INDEX.
                Var A : OTYPE[T1, T2].
                Var M : SET[m].
                Var a : BASIS[m] -> STYPE.
                Var v : BASIS[m] -> KTYPE[T2].
            )",
            "A (Sum i in USET[m], (a i) (v i))",
            "Sum i in USET[m], (a i) (A (v i))"
        },

/*
QCQI-2

Block[
 {DiracCtx = {v -> KType[T1], M -> SetType[m], lambda[_] -> SType, 
    w[_] -> KType[T1]}},
 DNEqQ[
  SuperDagger[v]\[SmallCircle]Sum[lambda[i] w[i], {i, M}],
  Sum[lambda[i] (SuperDagger[v]\[SmallCircle]w[i]), {i, M}]
  ]
 ]
*/
        {
            "QCQI-2",
            R"(
                Var T1 : INDEX.
                Var m : INDEX.
                Var M : SET[m].
                Var v : KTYPE[T1].
                Var lambda : BASIS[m] -> STYPE.
                Var w : BASIS[m] -> KTYPE[T1].
            )",
            "v^D Sum i in USET[m], (lambda i) (w i)",
            "Sum i in USET[m], (lambda i) (v^D (w i))"
        },

/*
QCQI-3

Block[
 {DiracCtx = {v -> KType[T], w -> KType[T]}},
 DNEqQ[
  SuperDagger[v]\[SmallCircle]w,
  SuperDagger[(SuperDagger[w]\[SmallCircle]v)]
  ]
 ]
*/
        {
            "QCQI-3",
            R"(
                Var T : INDEX.
                Var v : KTYPE[T].
                Var w : KTYPE[T].
            )",
            "v^D w",
            "(w^D v)^*"
        },
/*
QCQI-4

Block[
 {DiracCtx = {M -> SetType[m], lambda[_] -> SType, w[_] -> KType[T], 
    v -> KType[T]}},
 DNEqQ[
  ADJB[SUMK[i, M, lambda[i]~SCRK~w[i]]]~DOT~v,
  SUMS[i, M, CONJS[lambda[i]]~MLTS~(ADJB[w[i]]~DOT~v)]
  ]
 ]
*/
        {
            "QCQI-4",
            R"(
                Var T : INDEX.
                Var m : INDEX.
                Var M : SET[m].
                Var lambda : BASIS[m] -> STYPE.
                Var w : BASIS[m] -> KTYPE[T].
                Var v : KTYPE[T].
            )",
            "(Sum i in USET[m], (lambda i).(w i))^D v",
            "Sum i in USET[m], (lambda i)^* ((w i)^D v)"
        },

/*
QCQI-5

Block[
 {DiracCtx = {v[_] -> SType, w[_] -> SType}},
 DNEqQ[
  ADJB[SUMK[i, USET[T], v[i]~SCRK~KET[i]]]~DOT~
   SUMK[j, USET[T], w[j]~SCRK~KET[j]],
  SUMS[i, USET[T], CONJS[v[i]]~MLTS~w[i]]
  ]
 ]
*/

        {
            "QCQI-5",
            R"(
                Var T : INDEX.
                Var v : BASIS[T] -> STYPE.
                Var w : BASIS[T] -> STYPE.
            )",
            "(Sum i in USET[T], (v i).|i>)^D (Sum j in USET[T], (w j).|j>)",
            "Sum i in USET[T], (v i)^* (w i)"
        },

/*
QCQI-6

Block[
 {DiracCtx = {M -> SetType[T], v -> KType[T]}},
 DNEqQ[
  SUMO[i, M, KET[i]~OUTER~BRA[i]]~MLTK~v,
  SUMK[i, M, (BRA[i]~DOT~v)~SCRK~KET[i]]
  ]
 ]
*/

        {
            "QCQI-6",
            R"(
                Var T : INDEX.
                Var M : SET[T].
                Var v : KTYPE[T].
            )",
            "(Sum i in USET[T], |i> <i|) v",
            "Sum i in USET[T], (<i| v) |i>"
        },

/*
QCQI-7

DNEqQ[
 SUMO[i, USET[sigma], KET[i]~OUTER~BRA[i]],
 ONEO[sigma]
 ]
*/

        {
            "QCQI-7",
            R"(
                Var sigma : INDEX.
            )",
            "Sum i in USET[sigma], |i> <i|",
            "1O[sigma]"
        },

/*
QCQI-8

DNEqQ[
 SUMO[i, USET[sigma], KET[i]~OUTER~BRA[i]],
 ONEO[tau]
 ]
*/

        {
            "QCQI-8",
            R"(
                Var sigma : INDEX.
                Var tau : INDEX.
            )",
            "Sum i in USET[sigma], |i> <i|",
            "1O[tau]",
            false
        },

/*
QCQI-9

Block[
 {DiracCtx = {M -> SetType[m], T -> SetType[t], w[_] -> KType[T1], 
    A -> OType[T1, T2], v[_] -> KType[T2]}},
 DNEqQ[
  SUMO[i, M, 
   SUMO[j, T, (w[j]~OUTER~ADJB[w[j]])~MLTO~A~
     MLTO~(v[i]~OUTER~ADJB[v[i]])]],
  SUMO[i, M, 
   SUMO[j, T, (ADJB[w[j]]~DOT~(A~MLTK~v[i]))~
     SCRO~(w[j]~OUTER~ADJB[v[i]])]]
  ]
 ]
*/
        {
            "QCQI-9",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var m : INDEX.
                Var t : INDEX.
                Var M : SET[m].
                Var T : SET[t].
                Var w : BASIS[t] -> KTYPE[T1].
                Var A : OTYPE[T1, T2].
                Var v : BASIS[m] -> KTYPE[T2].
            )",
            "Sum i in M, Sum j in T, ((w j) (w j)^D) A ((v i) (v i)^D)",
            "Sum i in M, Sum j in T, ((w j)^D (A (v i))) . ((w j) (v i)^D)"
        },

/*
QCQI-10

Block[
 {DiracCtx = {v -> KType[T1], w -> KType[T2]}},
 DNEqQ[
  (ADJB[v]~DOT~v)~MLTS~(ADJB[w]~DOT~w),
  SUMS[i, 
   USET[T1], (ADJB[v]~DOT~KET[i])~MLTS~(BRA[i]~DOT~v)~
    MLTS~(ADJB[w]~DOT~w)]
  ]
 ]
*/

        {
            "QCQI-10",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var v : KTYPE[T1].
                Var w : KTYPE[T2].
            )",
            "(v^D v) * (w^D w)",
            "Sum i in USET[T1], (v^D |i>) * (<i| v) * (w^D w)"
        },

/*
QCQI-11

Block[
 {DiracCtx = {M -> SetType[m], a[_] -> SType, 
    A[_] -> OType[T1, T2]}},
 DNEqQ[
  ADJO[SUMO[i, M, a[i]~SCRO~A[i]]],
  SUMO[i, M, CONJS[a[i]]~SCRO~ADJO[A[i]]]
  ]
 ]
*/

        {
            "QCQI-11",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var m : INDEX.
                Var M : SET[m].
                Var a : BASIS[m] -> STYPE.
                Var A : BASIS[m] -> OTYPE[T1, T2].
            )",
            "(Sum i in M, (a i).(A i))^D",
            "Sum i in M, (a i)^* . (A i)^D"
        },

/*
QCQI-12

Block[
 {DiracCtx = {M -> SetType[m]}},
 With[
  {P = SUMO[i, M, KET[i]~OUTER~BRA[i]]},
  DNEqQ[
   P~MLTO~P,
   P
   ]
  ]
 ]
*/

        {
            "QCQI-12",
            R"(
                Var m : INDEX.
                Var M : SET[m].
                Def P := Sum i in M, |i> <i|.
            )",
            "P P",
            "P"
        },

/*
QCQI-13

Block[
 {DiracCtx = {M -> SetType[m], A -> OType[T1, R1], 
    B -> OType[T2, R2], a[_] -> SType, v[_] -> KType[R1], 
    w[_] -> KType[R2]}},
 DNEqQ[
  (A~TSRO~B)~MLTK~SUMK[i, M, a[i]~SCRK~(v[i]~TSRK~w[i])],
  SUMK[i, M, a[i]~SCRK~((A~MLTK~v[i])~TSRK~(B~MLTK~w[i]))]
  ]
 ]
*/

        {
            "QCQI-13",
            R"(
                Var T1 : INDEX.
                Var R1 : INDEX.
                Var T2 : INDEX.
                Var R2 : INDEX.
                Var m : INDEX.
                Var M : SET[m].
                Var A : OTYPE[T1, R1].
                Var B : OTYPE[T2, R2].
                Var a : BASIS[m] -> STYPE.
                Var v : BASIS[m] -> KTYPE[R1].
                Var w : BASIS[m] -> KTYPE[R2].
            )",
            "(A * B) (Sum i in M, (a i).((v i) (w i)))",
            "Sum i in M, (a i).((A (v i)) (B (w i)))"
        },

/*
QCQI-14

Block[
 {DiracCtx = {
    M -> SetType[m], T -> SetType[t],
    a[_] -> SType, b[_] -> SType,
    v[_] -> KType[T1], w[_] -> KType[T2],
    v2[_] -> KType[T1], w2[_] -> KType[T2]}},
 DNEqQ[
  (ADJB[SUMK[i, M, a[i]~SCRK~(v[i]~TSRK~w[i])]])~DOT~
   SUMK[j, T, b[j]~SCRK~(v2[j]~TSRK~w2[j])],
  SUMS[i, M, 
   SUMS[j, T, 
    MLTS[CONJS[a[i]], b[j], ADJB[v[i]]~DOT~v2[j], 
     ADJB[w[i]]~DOT~w2[j]]]]
  ]
 ]
*/
    
        {
            "QCQI-14",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var m : INDEX.
                Var t : INDEX.
                Var M : SET[m].
                Var T : SET[t].
                Var a : BASIS[m] -> STYPE.
                Var b : BASIS[t] -> STYPE.
                Var v : BASIS[m] -> KTYPE[T1].
                Var w : BASIS[m] -> KTYPE[T2].
                Var v2 : BASIS[t] -> KTYPE[T1].
                Var w2 : BASIS[t] -> KTYPE[T2].
            )",
            "((Sum i in M, (a i).((v i) (w i)))^D) (Sum j in T, (b j).((v2 j) (w2 j)))",
            "Sum i in M, Sum j in T, ((a i)^* * (b j) * ((v i)^D (v2 j))) ((w i)^D (w2 j))"
        },  

/*
QCQI-15

Block[
 {DiracCtx = {v -> KType[T], w -> KType[R]}},
 DNEqQ[
  SUMS[i, 
   USET[T], (ADJB[v]~DOT~KET[i])~MLTS~(BRA[i]~DOT~v)~
    MLTS~(ADJB[w]~DOT~w)],
  (ADJB[v]~DOT~v)~MLTS~(ADJB[w]~DOT~w)
  ]
 ]
*/

        {
            "QCQI-15",
            R"(
                Var T : INDEX.
                Var R : INDEX.
                Var v : KTYPE[T].
                Var w : KTYPE[R].
            )",
            "Sum i in USET[T], (v^D |i>) * (<i| v) * (w^D w)",
            "(v^D v) * (w^D w)"
        },

/*
QCQI-16

Block[
 {DiracCtx = {psi -> KType[T1], M[_] -> OType[T1, T1], m -> T2}},
 DNEqQ[
  SUMS[i, USET[T2], 
   SUMS[j, USET[
     T2], (((ADJB[psi]~MLTB~ADJO[M[m]])~TSRB~BRA[i])~
       MLTB~(ONEO[T1]~TSRO~(KET[m]~OUTER~BRA[m])))~
     DOT~((M[j]~MLTK~psi)~TSRK~KET[j])]],
  ADJB[psi]~DOT~(ADJO[M[m]]~MLTO~M[m]~MLTK~psi)
  ]
 ]
*/

        {
            "QCQI-16",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var psi : KTYPE[T1].
                Var M : BASIS[T2] -> OTYPE[T1, T1].
                Var m : BASIS[T2].
            )",
            "Sum i in USET[T2], Sum j in USET[T2], ((psi^D (M m)^D <i|) (1O[T1] * |m> <m|)) (((M j) psi) |j>)",
            "psi^D ((M m)^D (M m) psi)"
        },

// QCQI-17 omitted, complex scalar calculations

/*
QCQI-18

Block[
 {DiracCtx = {A -> OType[T, T]}},
 With[
  {sigma = SUMK[i, USET[T], KET[PAIR[i, i]]]},
  DNEqQ[
   (A~TSRO~ONEO[T])~MLTK~sigma, 
   (ONEO[T]~TSRO~TPO[A, T, T])~MLTK~sigma
   ]
  ]
 ]
*/

        {
            "QCQI-18",
            R"(
                Var T : INDEX.
                Var A : OTYPE[T, T].
                Def sigma := Sum i in USET[T], |(i, i)>.
            )",
            "(A * 1O[T]) sigma",
            "(1O[T] * (TPO T T A)) sigma"
        },

/*
COQQ-1 lftrace_baseE

Block[
 {DiracCtx = {A -> OType[T, T]}},
 DNEqQ[
  DNTr[A, T],
  SUMS[i, USET[T], BRA[i]~DOT~(A~MLTK~KET[i])]
  ]
 ]
*/

        {
            "COQQ-1 lftrace_baseE",
            R"(
                Var T : INDEX.
                Var A : OTYPE[T, T].
            )",
            "Tr T A",
            "Sum i in USET[T], <i| (A |i>)"
        },

/*
COQQ-2 lftraceC

Block[
 {DiracCtx = {f -> OType[T, T], g -> OType[T, T]}},
 DNEqQ[
  DNTr[f~MLTO~g, T],
  DNTr[g~MLTO~f, T]
  ]
 ]
*/
        {
            "COQQ-2 lftraceC",
            R"(
                Var T : INDEX.
                Var f : OTYPE[T, T].
                Var g : OTYPE[T, T].
            )",
            "Tr T (f g)",
            "Tr T (g f)"
        },

/*
COQQ-3 lftrace_is_linear

Block[
 {DiracCtx = {c -> SType, f -> OType[T, T], g -> OType[T, T]}},
 DNEqQ[
  DNTr[(c~SCRO~f)~ADDO~g, T],
  (c~MLTS~DNTr[f, T])~ADDS~DNTr[g, T]
  ]
 ]
*/
        
        {
            "COQQ-3 lftrace_is_linear",
            R"(
                Var T : INDEX.
                Var c : STYPE.
                Var f : OTYPE[T, T].
                Var g : OTYPE[T, T].
            )",
            "Tr T (c.f + g)",
            "c * (Tr T f) + Tr T g"
        },

/*
COQQ-4 lftrace_adj

Block[
 {DiracCtx = {f -> OType[T, T]}},
 DNEqQ[
  DNTr[ADJO[f], T],
  CONJS[DNTr[f, T]]
  ]
 ]
*/

        {
            "COQQ-4 lftrace_adj",
            R"(
                Var T : INDEX.
                Var f : OTYPE[T, T].
            )",
            "Tr T f^D",
            "(Tr T f)^*"
        },


/*
COQQ-5 lftrace_tr

Block[
 {DiracCtx = {f -> OType[T, T]}},
 DNEqQ[
  DNTr[TPO[f, T, T], T],
  DNTr[f, T]
  ]
 ]
*/
        
        {
            "COQQ-5 lftrace_tr",
            R"(
                Var T : INDEX.
                Var f : OTYPE[T, T].
            )",
            "Tr T (TPO T T f)",
            "Tr T f"
        },

/*
COQQ-6 lftrace_conj

Block[
 {DiracCtx = {f -> OType[T, T]}},
 DNEqQ[
  DNTr[CONJO[f, T, T], T],
  CONJS[DNTr[f, T]]
  ]
 ]
*/
        
        {
            "COQQ-6 lftrace_conj",
            R"(
                Var T : INDEX.
                Var f : OTYPE[T, T].
            )",
            "Tr T (CONJO T T f)",
            "(Tr T f)^*"
        },

/*
COQQ-7 outp_trlf

Block[
 {DiracCtx = {u -> KType[T], v -> BType[T]}},
 DNEqQ[
  DNTr[u~OUTER~v, T],
  v~DOT~u
  ]
 ]
*/

        {
            "COQQ-7 outp_trlf",
            R"(
                Var T : INDEX.
                Var u : KTYPE[T].
                Var v : BTYPE[T].
            )",
            "Tr T (u v)",
            "v u"
        },

/*
COQQ-8 sumeb_out

Block[
 {DiracCtx = {}},
 DNEqQ[
  SUMO[i, USET[T], KET[i]~OUTER~BRA[i]],
  ONEO[T]
  ]
 ]
*/
        
        {
            "COQQ-8 sumeb_out",
            R"(
                Var T : INDEX.
            )",
            "Sum i in USET[T], |i> <i|",
            "1O[T]"
        },

/*
COQQ-9 delta_lf_eb

Block[
 {DiracCtx = {i -> T1, j -> T2, k -> T2}},
 DNEqQ[
  (Ket[{i}]\[SmallCircle]Bra[{j}])\[SmallCircle]Ket[{k}],
  DELTA[k, j]  Ket[{i}]
  ]
 ]
*/

        {
            "COQQ-9 delta_lf_eb",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var i : BASIS[T1].
                Var j : BASIS[T2].
                Var k : BASIS[T2].
            )",
            "(|i> <j|) |k>",
            "DELTA[k, j] |i>"
        },

/*
COQQ-10 comp_delta_lf_cond

Block[
 {DiracCtx = {i -> T1, j -> T2, k -> T2, l -> T3}},
 DNEqQ[
  (KET[i]~OUTER~BRA[j])~MLTO~(KET[k]~OUTER~BRA[l]),
  DELTA[j, k]~SCRO~(KET[i]~OUTER~BRA[l])
  ]
 ]
*/
        
        {
            "COQQ-10 comp_delta_lf_cond",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var i : BASIS[T1].
                Var j : BASIS[T2].
                Var k : BASIS[T2].
                Var l : BASIS[T3].
            )",
            "(|i> <j|) (|k> <l|)",
            "DELTA[j, k] (|i> <l|)"
        },

/*
COQQ-11 comp_delta_lf

Block[
 {DiracCtx = {i -> T1, j -> T2, k -> T3}},
 DNEqQ[
  (KET[i]~OUTER~BRA[j])~MLTO~(KET[j]~OUTER~BRA[k]),
  KET[i]~OUTER~BRA[k]
  ]
 ]
*/
        
        {
            "COQQ-11 comp_delta_lf",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var i : BASIS[T1].
                Var j : BASIS[T2].
                Var k : BASIS[T3].
            )",
            "(|i> <j|) (|j> <k|)",
            "|i> <k|"
        },

/*
COQQ-12 trlf_deltar

Block[
 {DiracCtx = {f -> OType[T1, T2], i -> T2, j -> T1}},
 DNEqQ[
  DNTr[f~MLTO~(KET[i]~OUTER~BRA[j]), T1],
  BRA[j]~DOT~(f~MLTK~KET[i])
  ]
 ]
*/
        
        {
            "COQQ-12 trlf_deltar",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var f : OTYPE[T1, T2].
                Var i : BASIS[T2].
                Var j : BASIS[T1].
            )",
            "Tr T1 (f (|i> <j|))",
            "<j| (f |i>)"
        },

/*
COQQ-13 lfun_sum_delta

Block[
 {DiracCtx = {A -> OType[T1, T2]}},
 DNEqQ[
  SUMO[j, USET[T2], 
   SUMO[i, USET[T1], (BRA[i]~DOT~(A~MLTK~KET[j]))~
     SCRO~(KET[i]~OUTER~BRA[j])]],
  A
  ]
 ]
*/

        {
            "COQQ-13 lfun_sum_delta",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
            )",
            "Sum j in USET[T2], Sum i in USET[T1], (<i| (A |j>)) . (|i> <j|)",
            "A"
        },

/*
COQQ-14 onb_dot(CB)

Block[{DiracCtx = {i -> T, j -> T}},
 DNEqQ[Bra[{i}]\[SmallCircle]Ket[{j}], DELTA[i, j]]
 ]
*/

        {
            "COQQ-14 onb_dot(CB)",
            R"(
                Var T : INDEX.
                Var i : BASIS[T].
                Var j : BASIS[T].
            )",
            "<i| |j>",
            "DELTA[i, j]"
        },

/*
COQQ-15 onb_vec(CB)

Block[
 {DiracCtx = {v -> KType[T]}},
 DNEqQ[
  SUMK[i, USET[T], (BRA[i]~DOT~v)~SCRK~KET[i]],
  v
  ]
 ]
*/

        {
            "COQQ-15 onb_vec(CB)",
            R"(
                Var T : INDEX.
                Var v : KTYPE[T].
            )",
            "Sum i in USET[T], (<i| v) |i>",
            "v"
        },

/*
COQQ-16 outp_complV

Block[
 {DiracCtx = {A -> OType[T1, T2], u -> KType[T2], v -> BType[T3]}},
 DNEqQ[
  (A~MLTK~u)~OUTER~v,
  A~MLTO~(u~OUTER~v)
  ]
 ]
*/
        
        {
            "COQQ-16 outp_complV",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var A : OTYPE[T1, T2].
                Var u : KTYPE[T2].
                Var v : BTYPE[T3].
            )",
            "(A u) v",
            "A (u v)"
        },

/*
COQQ-17 outp_comprV

Block[
 {DiracCtx = {u -> KType[T1], v -> KType[T2], A -> OType[T3, T2]}},
 DNEqQ[
  u~OUTER~ADJB[A~MLTK~v],
  (u~OUTER~ADJB[v])~MLTO~ADJO[A]
  ]
 ]
*/
        
        {
            "COQQ-17 outp_comprV",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var u : KTYPE[T1].
                Var v : KTYPE[T2].
                Var A : OTYPE[T3, T2].
            )",
            "u (A v)^D",
            "(u v^D) A^D"
        },

/*
COQQ-18 onb_lfun(CB)

Block[{DiracCtx = {A -> OType[T1, T2]}},
 DNEqQ[A, 
  SUMO[i, USET[T2], (A\[SmallCircle]Ket[{i}])\[SmallCircle]Bra[{i}]]]
 ]
*/

        {
            "COQQ-18 onb_lfun(CB)",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
            )",
            "A",
            "Sum i in USET[T2], (A |i>) <i|"
        },


///////////////////////////////////////////////
// this example needs concrete basis and is not implemented



/*
COQQ-19 sumonb_out_bool(CB)
Block[{DiracCtx = {0 -> {0, 1}, 1 -> {0, 1}}},
 DNEqQ[Ket[{0}]\[SmallCircle]Bra[{0}] + 
   Ket[{1}]\[SmallCircle]Bra[{1}], ONEO[{0, 1}]]
 ]
*/

        // {
        //     "COQQ-19 sumonb_out_bool(CB)",
        //     R"(
        //         Var qubit : INDEX.
        //         Var #0 : BASIS[qubit].
        //         Var #1 : BASIS[qubit].
        //     )",
        //     "|#0> <#0| + |#1> <#1|",
        //     "1O[qubit]"
        // },


/*
COQQ-20 ponb_ns(CB)

Block[{DiracCtx = {i -> T}},
 DNEqQ[Bra[{i}]\[SmallCircle]Ket[{i}], CPX[1]]
 ]
*/

        {
            "COQQ-20 ponb_ns(CB)",
            R"(
                Var T : INDEX.
                Var i : BASIS[T].
            )",
            "<i| |i>",
            "1"
        },

/*
COQQ-21 linear_tensmx

Block[
 {DiracCtx = {a -> SType, A -> OType[T1, T2], B -> OType[T3, T4], 
    D -> OType[T3, T4]}},
 DNEqQ[
  A\[CircleTimes]((a B) ~ADDO~ D),
  (a (A\[CircleTimes]B))~ADDO~(A\[CircleTimes]D)
  ]
 ]
*/

        {
            "COQQ-21 linear_tensmx",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var a : STYPE.
                Var A : OTYPE[T1, T2].
                Var B : OTYPE[T3, T4].
                Var D : OTYPE[T3, T4].
            )",
            "A * ((a B) + D)",
            "(a (A * B)) + (A * D)"
        },

/*
COQQ-22 linear_tensmxr

Block[
 {DiracCtx = {a -> SType, A -> OType[T1, T2], B -> OType[T1, T2], 
    D -> OType[T3, T4]}},
 DNEqQ[
  ((a A)~ADDO~ B)\[CircleTimes]D,
  (a (A\[CircleTimes]D))~ADDO~(B\[CircleTimes]D)
  ]
 ]
*/

        {
            "COQQ-22 linear_tensmxr",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var a : STYPE.
                Var A : OTYPE[T1, T2].
                Var B : OTYPE[T1, T2].
                Var D : OTYPE[T3, T4].
            )",
            "((a A) + B) * D",
            "(a (A * D)) + (B * D)"
        },

/*
COQQ-23 adjmx_tens

Block[
 {DiracCtx = {A -> OType[T1, T2], B -> OType[T3, T4]}},
 DNEqQ[
  ADJO[A~TSRO~B],
  ADJO[A]~TSRO~ADJO[B]
  ]
 ]
*/

        {
            "COQQ-23 adjmx_tens",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var A : OTYPE[T1, T2].
                Var B : OTYPE[T3, T4].
            )",
            "(A * B)^D",
            "A^D * B^D"
        },

/*
COQQ-24 mxtrace_tens

Block[
 {DiracCtx = {A -> OType[T1, T1], B -> OType[T2, T2]}},
 DNEqQ[
  DNTr[A~TSRO~B, T1~ProdType~T2],
  DNTr[A, T1]~MLTS~DNTr[B, T2]
  ]
 ]
*/
        
        {
            "COQQ-24 mxtrace_tens",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T1].
                Var B : OTYPE[T2, T2].
            )",
            "Tr (T1 * T2) (A * B)",
            "(Tr T1 A) * (Tr T2 B)"
        },

/*
COQQ-25 tr_tens

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T1~ProdType~T2]}},
 DNEqQ[
  DNTr[A, T1~ProdType~T2],
  Sum[Sum[
    Bra[{PAIR[i, j]}]\[SmallCircle]A\[SmallCircle]Ket[{PAIR[i, 
        j]}], {i, USET[T1]}], {j, USET[T2]}]
  ]
 ]
*/
    
        {
            "COQQ-25 tr_tens",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1 * T2, T1 * T2].
            )",
            "Tr (T1 * T2) A",
            "Sum i in USET[T1], Sum j in USET[T2], <(i, j)| A |(i, j)>"
        },

/*
COQQ-26 mxswapK

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T3~ProdType~T4]}},
 DNEqQ[
  SWAP[SWAP[A, T1, T2, T3, T4], T2, T1, T4, T3],
  A
  ]
 ]
*/

        {
            "COQQ-26 mxswapK",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var A : OTYPE[T1 * T2, T3 * T4].
            )",
            "SWAP T2 T1 T4 T3 (SWAP T1 T2 T3 T4 A)",
            "A"
        },

/*
COQQ-27 mxswap_is_linear
Block[
 {DiracCtx = {a -> SType,
    x -> OType[T1~ProdType~T2, T3~ProdType~T4],
     y -> OType[T1~ProdType~T2, T3~ProdType~T4]}},
 DNEqQ[
  SWAP[(a~SCRO~x)~ADDO~y, T1, T2, T3, T4],
  (a~SCRO~SWAP[x, T1, T2, T3, T4])~ADDO~SWAP[y, T1, T2, T3, T4]
  ]
 ]
*/

        {
            "COQQ-27 mxswap_is_linear",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var a : STYPE.
                Var x : OTYPE[T1 * T2, T3 * T4].
                Var y : OTYPE[T1 * T2, T3 * T4].
            )",
            "SWAP T1 T2 T3 T4 ((a x) + y)",
            "a (SWAP T1 T2 T3 T4 x) + SWAP T1 T2 T3 T4 y"
        },

/*
COQQ-28 mxswap_tens

Block[
 {DiracCtx = {A -> OType[T1, T2], B -> OType[T3, T4]}},
 DNEqQ[
  SWAP[A~TSRO~B, T1, T3, T2, T4],
  B~TSRO~A
  ]
 ]
*/

        {
            "COQQ-28 mxswap_tens",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var A : OTYPE[T1, T2].
                Var B : OTYPE[T3, T4].
            )",
            "SWAP T1 T3 T2 T4 (A * B)",
            "B * A"
        },

/*
COQQ-29 mxswap_trace

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T1~ProdType~T2]}},
 DNEqQ[
  DNTr[SWAP[A, T1, T2, T1, T2], T2~ProdType~T1],
  DNTr[A, T1~ProdType~T2]
  ]
 ]
*/
        {
            "COQQ-29 mxswap_trace",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1 * T2, T1 * T2].
            )",
            "Tr (T2 * T1) (SWAP T1 T2 T1 T2 A)",
            "Tr (T1 * T2) A"
        },

/*
COQQ-30 mxswap_mul

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T3~ProdType~T4], 
    B -> OType[T3~ProdType~T4, T5~ProdType~T6]}},
 DNEqQ[
  SWAP[A, T1, T2, T3, T4]~MLTO~SWAP[B, T3, T4, T5, T6],
  SWAP[A~MLTO~B, T1, T2, T5, T6]
  ]
 ]
*/

        {
            "COQQ-30 mxswap_mul",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var T5 : INDEX.
                Var T6 : INDEX.
                Var A : OTYPE[T1 * T2, T3 * T4].
                Var B : OTYPE[T3 * T4, T5 * T6].
            )",
            "(SWAP T1 T2 T3 T4 A) (SWAP T3 T4 T5 T6 B)",
            "SWAP T1 T2 T5 T6 (A B)"
        },

/*
COQQ-31 mxswap_trmx

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T3~ProdType~T4]}},
 DNEqQ[
  TPO[SWAP[A, T1, T2, T3, T4], T2~ProdType~T1, T4~ProdType~T3],
  SWAP[TPO[A, T1~ProdType~T2, T3~ProdType~T4], T3, T4, T1, T2]
  ]
 ]
*/

        {
            "COQQ-31 mxswap_trmx",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var A : OTYPE[T1 * T2, T3 * T4].
            )",
            "TPO (T2 * T1) (T4 * T3) (SWAP T1 T2 T3 T4 A)",
            "SWAP T3 T4 T1 T2 (TPO (T1 * T2) (T3 * T4) A)"
        },

/*
COQQ-32 mxswap_trmxC

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T3~ProdType~T4]}},
 DNEqQ[
  ADJO[SWAP[A, T1, T2, T3, T4]],
  SWAP[ADJO[A], T3, T4, T1, T2]
  ]
 ]
*/

        {
            "COQQ-32 mxswap_trmxC",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var A : OTYPE[T1 * T2, T3 * T4].
            )",
            "(SWAP T1 T2 T3 T4 A)^D",
            "SWAP T3 T4 T1 T2 A^D"
        },

/*
COQQ-33 ptrace2E1

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T3~ProdType~T2]}},
 DNEqQ[
  DNPTr2[A, T2, T1, T3],
  DNPTr1[SWAP[A, T1, T2, T3, T2], T2, T1, T3]
  ]
 ]
*/

        {
            "COQQ-33 ptrace2E1",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var A : OTYPE[T1 * T2, T3 * T2].
            )",
            "PTr2 T2 T1 T3 A",
            "PTr1 T2 T1 T3 (SWAP T1 T2 T3 T2 A)"
        },

/*
COQQ-34 ptrace1E2

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T1~ProdType~T3]}},
 DNEqQ[
  DNPTr1[A, T1, T2, T3],
  DNPTr2[SWAP[A, T1, T2, T1, T3], T1, T2, T3]
  ]
 ]
*/
        
        {
            "COQQ-34 ptrace1E2",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var A : OTYPE[T1 * T2, T1 * T3].
            )",
            "PTr1 T1 T2 T3 A",
            "PTr2 T1 T2 T3 (SWAP T1 T2 T1 T3 A)"
        },

/*
COQQ-35 ptrace2_is_linear

Block[
 {DiracCtx = {c -> SType,
    A -> OType[T1~ProdType~T2, T3~ProdType~T2],
    B -> OType[T1~ProdType~T2, T3~ProdType~T2]}},
 DNEqQ[
  DNPTr2[(c~SCRO~A)~ADDO~B, T2, T1, T3],
  (c~SCRO~DNPTr2[A, T2, T1, T3])~ADDO~DNPTr2[B, T2, T1, T3]
  ]
 ]
*/

        {
            "COQQ-35 ptrace2_is_linear",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var c : STYPE.
                Var A : OTYPE[T1 * T2, T3 * T2].
                Var B : OTYPE[T1 * T2, T3 * T2].
            )",
            "PTr2 T2 T1 T3 ((c A) + B)",
            "c (PTr2 T2 T1 T3 A) + PTr2 T2 T1 T3 B"
        },

/*
COQQ-36 ptrace1_is_linear

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T1~ProdType~T3], 
    B -> OType[T1~ProdType~T2, T1~ProdType~T3], c -> SType}},
 DNEqQ[
  DNPTr1[(c A) ~ADDO~ B, T1, T2, T3],
  (c DNPTr1[A, T1, T2, T3])~ADDO~DNPTr1[B, T1, T2, T3]
  ]
 ]
*/

        {
            "COQQ-36 ptrace1_is_linear",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var c : STYPE.
                Var A : OTYPE[T1 * T2, T1 * T3].
                Var B : OTYPE[T1 * T2, T1 * T3].
            )",
            "PTr1 T1 T2 T3 ((c A) + B)",
            "c (PTr1 T1 T2 T3 A) + PTr1 T1 T2 T3 B"
        },

/*
COQQ-37 tr_ptrace2

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T1~ProdType~T2]}},
 DNEqQ[
  DNTr[A, T1~ProdType~T2],
  DNTr[DNPTr2[A, T2, T1, T1], T1]
  ]
 ]
*/

        {
            "COQQ-37 tr_ptrace2",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1 * T2, T1 * T2].
            )",
            "Tr (T1 * T2) A",
            "Tr T1 (PTr2 T2 T1 T1 A)"
        },

/*
COQQ-38 tr_ptrace1

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T1~ProdType~T2]}},
 DNEqQ[
  DNTr[A, T1~ProdType~T2],
  DNTr[DNPTr1[A, T1, T2, T2], T2]
  ]
 ]
*/

        {
            "COQQ-38 tr_ptrace1",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1 * T2, T1 * T2].
            )",
            "Tr (T1 * T2) A",
            "Tr T2 (PTr1 T1 T2 T2 A)"
        },

/*
COQQ-39 ptrace1_mul_tens1mx

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T1~ProdType~T3], 
    B -> OType[T3, T4]}},
 DNEqQ[
  DNPTr1[A~MLTO~(ONEO[T1]~TSRO~B), T1, T2, T4],
  DNPTr1[A, T1, T2, T3]~MLTO~B
  ]
 ]
*/

        {
            "COQQ-39 ptrace1_mul_tens1mx",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var A : OTYPE[T1 * T2, T1 * T3].
                Var B : OTYPE[T3, T4].
            )",
            "PTr1 T1 T2 T4 (A (1O[T1] * B))",
            "(PTr1 T1 T2 T3 A) B"
        },

/*
COQQ-40 tensmx11

Block[
 {DiracCtx = {}},
 DNEqQ[
  ONEO[T1]~TSRO~ONEO[T2],
  ONEO[T1~ProdType~T2]
  ]
 ]
*/

        {
            "COQQ-40 tensmx11",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
            )",
            "1O[T1] * 1O[T2]",
            "1O[T1 * T2]"
        },

/*
COQQ-41 tensmxE_mid

Block[
 {DiracCtx = {i -> T1, A -> OType[T1, T2~ProdType~T2p], 
    B -> OType[T2~ProdType~T2p, T3], j -> T3}},
 DNEqQ[
  BRA[i]~DOT~((A~MLTO~B)~MLTK~KET[j]), 
  SUMS[i1, USET[T2], 
   SUMS[i2, 
    USET[T2p], (BRA[i]~DOT~(A~MLTK~KET[PAIR[i1, i2]]))~
     MLTS~(BRA[PAIR[i1, i2]]~DOT~(B~MLTK~KET[j]))]]
  ]
 ]
*/

        {
            "COQQ-41 tensmxE_mid",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T2p : INDEX.
                Var T3 : INDEX.
                Var i : BASIS[T1].
                Var A : OTYPE[T1, T2 * T2p].
                Var B : OTYPE[T2 * T2p, T3].
                Var j : BASIS[T3].
            )",
            "<i| ((A B) |j>)",
            "Sum i1 in USET[T2], Sum i2 in USET[T2p], (<i| (A |(i1, i2)>) * <(i1, i2)| (B |j>))"
        },

/*
COQQ-42 tens_delta_mx1_mulEl

Block[
 {DiracCtx = {k -> T1, p -> T2, i -> T1, j -> T3, 
    A -> OType[T3~ProdType~T2, T5], q -> T5}},
 DNEqQ[
  BRA[PAIR[k, p]]~
   DOT~(((KET[i]~OUTER~BRA[j]~TSRO~ONEO[T2])~MLTO~A)~MLTK~KET[q]),
  DELTA[i, k]~MLTS~(BRA[PAIR[j, p]]~DOT~(A~MLTK~KET[q]))
  ]
 ]
*/

        {
            "COQQ-42 tens_delta_mx1_mulEl",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T5 : INDEX.
                Var k : BASIS[T1].
                Var p : BASIS[T2].
                Var i : BASIS[T1].
                Var j : BASIS[T3].
                Var A : OTYPE[T3 * T2, T5].
                Var q : BASIS[T5].
            )",
            "<(k, p)| ((|i> <j| * 1O[T2]) A |q>)",
            "DELTA[i, k] * (<(j, p)| (A |q>))"
        },

/*
COQQ-43 tens_delta_mx1_mulEr

Block[
 {DiracCtx = {p -> T1, A -> OType[T1, T2~ProdType~T3], i -> T2, 
    j -> T4, k -> T4, q -> T3}},
 DNEqQ[
  BRA[p]~
   DOT~((A~MLTO~((KET[i]~OUTER~BRA[j])~TSRO~ONEO[T3]))~MLTK~
     KET[PAIR[k, q]]),
  DELTA[j, k]~MLTS~(BRA[p]~DOT~(A~MLTK~KET[PAIR[i, q]]))
  ]
 ]
*/

        {
            "COQQ-43 tens_delta_mx1_mulEr",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var p : BASIS[T1].
                Var A : OTYPE[T1, T2 * T3].
                Var i : BASIS[T2].
                Var j : BASIS[T4].
                Var k : BASIS[T4].
                Var q : BASIS[T3].
            )",
            "<p| (A ((|i> <j|) * 1O[T3]) |(k, q)>)",
            "DELTA[j, k] * (<p| (A |(i, q)>))"
        },

/*
COQQ-44 diag_mx_tens

Block[{DiracCtx = {K1 -> KType[T1], K2 -> KType[T2]}},
 DNEqQ[diagmx[K1\[CircleTimes]K2, T1~ProdType~T2], 
  diagmx[K1, T1]\[CircleTimes]diagmx[K2, T2]]
 ]
*/

        {
            "COQQ-44 diag_mx_tens",
            R"(
                (* diagonal matrix 
                    diagmx[K_,T_]:=Module[{i}, SUMO[IDX[{i, USET[T]}], (Bra[{i}]\[SmallCircle]K)Ket[{i}]\[SmallCircle]Bra[{i}]]];
                *)
                Def diagmx := idx sigma => fun K : KTYPE[sigma] => Sum i in USET[sigma], (<i| K) . |i> <i|.

                Var T1 : INDEX.
                Var T2 : INDEX.
                Var K1 : KTYPE[T1].
                Var K2 : KTYPE[T2].
            )",
            "diagmx (T1 * T2) (K1 K2)",
            "(diagmx T1 K1) * (diagmx T2 K2)"
        },

/*
COQQ-45 ptrace2_mulmxI

Block[
 {DiracCtx = {A -> OType[T1~ProdType~T2, T3~ProdType~T2], 
    B -> OType[T3, T4]}},
 DNEqQ[
  DNPTr2[A~MLTO~(B~TSRO~ONEO[T2]), T2, T1, T4],
  DNPTr2[A, T2, T1, T3]~MLTO~B
  ]
 ]
*/
        
        {
            "COQQ-45 ptrace2_mulmxI",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var A : OTYPE[T1 * T2, T3 * T2].
                Var B : OTYPE[T3, T4].
            )",
            "PTr2 T2 T1 T4 (A (B * 1O[T2]))",
            "(PTr2 T2 T1 T3 A) B"
        },

/*
COQQ-46 mulmx_diag_colrow

Block[
 {DiracCtx = {K -> KType[T], B -> OType[T, T], A -> OType[T, T]}},
 DNEqQ[
  A\[SmallCircle]diagmx[K, T]\[SmallCircle]B,
  Sum[(Bra[{i}] \[SmallCircle]K)  (A \[SmallCircle] 
       Ket[{i}]) \[SmallCircle] (Bra[{i}] \[SmallCircle] B), {i, 
    USET[T]}]
  ]
 ]
*/
    
        {
            "COQQ-46 mulmx_diag_colrow",
            R"(
                (* diagonal matrix 
                    diagmx[K_,T_]:=Module[{i}, SUMO[IDX[{i, USET[T]}], (Bra[{i}]\[SmallCircle]K)Ket[{i}]\[SmallCircle]Bra[{i}]]];
                *)
                Def diagmx := idx sigma => fun K : KTYPE[sigma] => Sum i in USET[sigma], (<i| K) . |i> <i|.

                Var T : INDEX.
                Var K : KTYPE[T].
                Var B : OTYPE[T, T].
                Var A : OTYPE[T, T].
            )",
            "A (diagmx T K) B",
            "Sum i in USET[T], ((<i| K) (A |i>) <i| B))"
        },

/*
COQQ-47 cplmtE

Block[
 {DiracCtx = {A -> OType[T, T]}},
 DNEqQ[
  ONEO[T]~ADDO~(CPX[-1]~SCRO~A),
  cplmt[A, T]
  ]
 ]
*/
    
        // {
        //     "COQQ-47 cplmtE",
        //     R"(
        //         Var T : INDEX.
        //         Var A : OTYPE[T, T].
        //     )",
        //     "1O[T] + (-1 * A)",
        //     "cplmt A T"
        // },

/*
COQQ-48 cplmtK

Block[
 {DiracCtx = {A -> OType[T, T]}},
 DNEqQ[
  cplmt[cplmt[A, T], T],
  A
  ]
 ]
*/

        // {
        //     "COQQ-48 cplmtK",
        //     R"(
        //         (* 
        //             cplmt[A_, T_]:= ONEO[T] ~ADDO~ (CPX[-1]~SCRO~A);
        //         *)

        //         Var T : INDEX.
        //         Var A : OTYPE[T, T].
        //     )",
        //     "cplmt (cplmt A T) T",
        //     "A"
        // },

/*
COQQ-49 cplmt1

Block[
 {DiracCtx = {}},
 DNEqQ[
  cplmt[ONEO[T], T],
  ZEROO[T, T]
  ]
 ]
*/

        // {
        //     "COQQ-49 cplmt1",
        //     R"(
        //         (* 
        //             cplmt[A_, T_]:= ONEO[T] ~ADDO~ (CPX[-1]~SCRO~A);
        //         *)

        //         Var T : INDEX.
        //     )",
        //     "cplmt 1O[T] T",
        //     "0O[T, T]"
        // },

/*
COQQ-50 cplmt0

Block[
 {DiracCtx = {}},
 DNEqQ[
  cplmt[ZEROO[T, T], T],
  ONEO[T]
  ]
 ]
*/
    
        // {
        //     "COQQ-50 cplmt0",
        //     R"(
        //         (* 
        //             cplmt[A_, T_]:= ONEO[T] ~ADDO~ (CPX[-1]~SCRO~A);
        //         *)

        //         Var T : INDEX.
        //     )",
        //     "cplmt 0O[T, T] T",
        //     "1O[T]"
        // },



/*
COQQ-51 formlf_comp

Block[
 {DiracCtx = {A -> OType[T1, T2], B -> OType[T2, T3], 
    X -> OType[T3, T3]}},
 DNEqQ[
  formlf[A, formlf[B, X]],
  formlf[A~MLTO~B, X]
  ]
 ]
*/
    
        {
            "COQQ-51 formlf_comp",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var A : OTYPE[T1, T2].
                Var B : OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].
            )",
            "formlf T1 T2 A (formlf T2 T3 B X)",
            "formlf T1 T3 (A B) X"
        },

/*
COQQ-52 formlf_adj

Block[
 {DiracCtx = {A -> OType[T1, T2], X -> OType[T2, T2]}},
 DNEqQ[
  SuperDagger[formlf[A, X]],
  formso[A][SuperDagger[X]]
  ]
 ]
*/

        {
            "COQQ-52 formlf_adj",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
            )",
            "(formlf T1 T2 A X)^D",
            "formso T1 T2 A (X^D)"
        },

/*
COQQ-53 formlf1E

Block[
 {DiracCtx = {A -> OType[T1, T2]}},
 DNEqQ[
  formlf[A, ONEO[T2]],
  A \[SmallCircle] (SuperDagger[A])
  ]
 ]
*/
        
        {
            "COQQ-53 formlf1E",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
            )",
            "formlf T1 T2 A 1O[T2]",
            "A (A^D)"
        },

/*
COQQ-54 formlf1EV

Block[
 {DiracCtx = {A -> OType[T1, T2]}},
 DNEqQ[
  formlf[SuperDagger[A], ONEO[T1]],
  SuperDagger[A] \[SmallCircle] A
  ]
 ]
*/

        {
            "COQQ-54 formlf1EV",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
            )",
            "formlf T2 T1 (A^D) 1O[T1]",
            "(A^D) A"
        },

/*
COQQ-55 formlfE

Block[
 {DiracCtx = {A -> OType[T1, T2], X -> OType[T2, T2]}},
 DNEqQ[
  formlf[A, X],
  A\[SmallCircle]X\[SmallCircle]SuperDagger[A]
  ]
 ]
*/

        {
            "COQQ-55 formlfE",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
            )",
            "formlf T1 T2 A X",
            "A X A^D"
        },

/*
COQQ-56 formlfEV

Block[
 {DiracCtx = {A -> OType[T1, T2], X -> OType[T1, T1]}},
 DNEqQ[
  formlf[SuperDagger[A], X],
  SuperDagger[A]\[SmallCircle]X\[SmallCircle]A
  ]
 ]
*/

        {
            "COQQ-56 formlfEV",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
                Var X : OTYPE[T1, T1].
            )",
            "formlf T2 T1 (A^D) X",
            "(A^D) X A"
        },

/*
COQQ-57 formlf_linear

Block[
 {DiracCtx = {A -> OType[T1, T2], X -> OType[T2, T2], 
    Y -> OType[T2, T2], c -> SType}},
 DNEqQ[
  formlf[A, (c X) ~ADDO~ Y],
  (c formlf[A, X])~ADDO~formlf[A, Y]
  ]
 ]
*/

        {
            "COQQ-57 formlf_linear",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
                Var Y : OTYPE[T2, T2].
                Var c : STYPE.
            )",
            "formlf T1 T2 A ((c X) + Y)",
            "c (formlf T1 T2 A X) + formlf T1 T2 A Y"
        },

/*
COQQ-58 superop_is_linear

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2], Y -> OType[T2, T2], 
    a -> SType}},
 E1 = superop[M, e, f];
 DNEqQ[
  E1[a X~ADDO~Y],
  a E1[X] ~ADDO~ E1[Y]
  ]
 ]
*/

        {
            "COQQ-58 superop_is_linear",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
                Var Y : OTYPE[T2, T2].
                Var a : STYPE.
                Def E1 := superop S M T1 T2 e f.
            )",
            "E1 (a X + Y)",
            "a (E1 X) + E1 Y"
        },

/*
COQQ-59 addsoA

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T1, T2], 
    f2[_] -> OType[T1, T2],
    M3 -> SetType[S3], e3[_] -> OType[T1, T2], f3[_] -> OType[T1, T2],
     X -> OType[T2, T2]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E3 = superop[M3, e3, f3];
 DNEqQ[
  addso[E1, addso[E2, E3]][X],
  addso[addso[E1, E2], E3][X]
  ]
 ]
*/

        {
            "COQQ-59 addsoA",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T1, T2].
                Var f2 : BASIS[S2]->OTYPE[T1, T2].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T1, T2].
                Var f3 : BASIS[S3]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T1 T2 e2 f2.
                Def E3 := superop S3 M3 T1 T2 e3 f3.
            )",
            "addso T1 T2 E1 (addso T1 T2 E2 E3) X",
            "addso T1 T2 (addso T1 T2 E1 E2) E3 X"
        },

/*
COQQ-60 addsoC

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T1, T2], f2[_] -> OType[T1, T2],
     X -> OType[T2, T2]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  addso[E1, E2][X],
  addso[E2, E1][X]
  ]
 ]
*/

        {
            "COQQ-60 addsoC",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T1, T2].
                Var f2 : BASIS[S2]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T1 T2 e2 f2.
            )",
            "addso T1 T2 E1 E2 X",
            "addso T1 T2 E2 E1 X"
        },

/*
COQQ-61 add0so

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  addso[abortso[T1, T2], E1][X],
  E1[X]
  ]
 ]
*/

        {
            "COQQ-61 add0so",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S M T1 T2 e f.
            )",
            "addso T1 T2 (abortso T1 T2) E1 X",
            "E1 X"
        },

/*
COQQ-62 addNso

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  addso[oppso[E1], E1][X],
  abortso[T1, T2][X]
  ]
 ]
*/

        // {
        //     "COQQ-62 addNso",
        //     R"(
        //         Var S : INDEX.
        //         Var T1 : INDEX.
        //         Var T2 : INDEX.
        //         Var M : SET[S].
        //         Var e : BASIS[S]->OTYPE[T1, T2].
        //         Var f : BASIS[S]->OTYPE[T1, T2].
        //         Var X : OTYPE[T2, T2].

        //         Def E1 := superop S M T1 T2 e f.
        //     )",
        //     "addso T1 T2 (oppso T1 T2 E1) E1 X",
        //     "abortso T1 T2 X"
        // },

/*
COQQ-63 scale1so

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  scaleso[CPX[1], E1][X],
  E1[X]
  ]
 ]
*/
        
        {
            "COQQ-63 scale1so",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S M T1 T2 e f.
            )",
            "scaleso T1 T2 1 E1 X",
            "E1 X"
        },

/*
COQQ-64 scalesoDl

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2], a -> SType, 
    b -> SType}},
 E1 = superop[M, e, f];
 DNEqQ[
  scaleso[a~ADDS~b, E1][X],
  addso[scaleso[a, E1], scaleso[b, E1]][X]
  ]
 ]
*/

        {
            "COQQ-64 scalesoDl",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.
                Var b : STYPE.

                Def E1 := superop S M T1 T2 e f.
            )",
            "scaleso T1 T2 (a + b) E1 X",
            "addso T1 T2 (scaleso T1 T2 a E1) (scaleso T1 T2 b E1) X"
        },


/*
COQQ-65 scalesoDr

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2], M2 -> SetType[S2], e2[_] -> OType[T1, T2],
     f2[_] -> OType[T1, T2], X -> OType[T2, T2], a -> SType}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  scaleso[a, addso[E1, E2]][X],
  addso[scaleso[a, E1], scaleso[a, E2]][X]
  ]
 ]
*/

        {
            "COQQ-65 scalesoDr",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T1, T2].
                Var f2 : BASIS[S2]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T1 T2 e2 f2.
            )",
            "scaleso T1 T2 a (addso T1 T2 E1 E2) X",
            "addso T1 T2 (scaleso T1 T2 a E1) (scaleso T1 T2 a E2) X"
        },

/*
COQQ-66 scalesoA

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2], a -> SType, 
    b -> SType}},
 E1 = superop[M, e, f];
 DNEqQ[
  scaleso[a, scaleso[b, E1]][X],
  scaleso[a~MLTS~b, E1][X]
  ]
 ]
*/

        {
            "COQQ-66 scalesoA",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.
                Var b : STYPE.

                Def E1 := superop S M T1 T2 e f.
            )",
            "scaleso T1 T2 a (scaleso T1 T2 b E1) X",
            "scaleso T1 T2 (a * b) E1 X"
        },

/*
COQQ-67 abort_soE

Block[
 {DiracCtx = {X -> OType[T2, T2]}},
 DNEqQ[
  abortso[T1, T2][X],
  ZEROO[T1, T1]
  ]
 ]
*/

        {
            "COQQ-67 abort_soE",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var X : OTYPE[T2, T2].
            )",
            "abortso T1 T2 X",
            "0O[T1, T1]"
        },

/*
COQQ-68 add_soE

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T1, T2], f2[_] -> OType[T1, T2],
     X -> OType[T2, T2]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  addso[E1, E2][X],
  E1[X]~ADDO~E2[X]
  ]
 ]
*/
        
        {
            "COQQ-68 add_soE",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T1, T2].
                Var f2 : BASIS[S2]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T1 T2 e2 f2.
            )",
            "addso T1 T2 E1 E2 X",
            "E1 X + E2 X"
        },

/*
COQQ-69 opp_soE

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  oppso[E1][X],
  CPX[-1]~SCRO~E1[X]
  ]
 ]
*/

        // {
        //     "COQQ-69 opp_soE",
        //     R"(
        //         Var S : INDEX.
        //         Var T1 : INDEX.
        //         Var T2 : INDEX.
        //         Var M : SET[S].
        //         Var e : BASIS[S]->OTYPE[T1, T2].
        //         Var f : BASIS[S]->OTYPE[T1, T2].
        //         Var X : OTYPE[T2, T2].

        //         Def E1 := superop S M T1 T2 e f.
        //     )",
        //     "oppso T1 T2 E1 X",
        //     "(-1 * E1 X)"
        // },

/*
COQQ-70 sum_soE

Block[{DiracCtx = {M -> SetType[Sm], Ni[_] -> SetType[Sn], 
    fl[_][_] -> OType[T1, T2], fr[_][_] -> OType[T1, T2], 
    X -> OType[T2, T2]}},
 DNEqQ[sumso[M, superop[Ni[#], fl[#], fr[#]] &][X],
  SUMO[i, M, superop[Ni[i], fl[i], fr[i]][X]]]
 ]
*/

        {
            "COQQ-70 sum_soE",
            R"(
                Var Sm : INDEX.
                Var Sn : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[Sm].
                Var Ni : BASIS[Sm]->SET[Sn].
                Var fl : BASIS[Sm]->BASIS[Sn]->OTYPE[T1, T2].
                Var fr : BASIS[Sm]->BASIS[Sn]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
            )",
            "sumso Sm M T1 T2 (fun i : BASIS[Sm] => superop Sn (Ni i) T1 T2 (fl i) (fr i)) X",
            "Sum i in M, superop Sn (Ni i) T1 T2 (fl i) (fr i) X"
        },

/*
COQQ-71 scale_soE

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2], c -> SType}},
 E1 = superop[M, e, f];
 DNEqQ[
  scaleso[c, E1][X],
  c~SCRO~E1[X]
  ]
 ]
*/

        {
            "COQQ-71 scale_soE",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].
                Var c : STYPE.

                Def E1 := superop S M T1 T2 e f.
            )",
            "scaleso T1 T2 c E1 X",
            "c . (E1 X)"
        },

/*
COQQ-72 comp_soElr

Block[{DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], 
    f2[_] -> OType[T2, T3], X -> OType[T3, T3]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[compso[E1, E2][X], compsor[E2, E1][X]]
 ]
*/

        {
            "COQQ-72 comp_soElr",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
            )",
            "compso T1 T2 T3 E1 E2 X",
            "compsor T1 T2 T3 E2 E1 X"
        },

/*
COQQ-73 comp_soErl

Block[{DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T2, T1], 
    f1[_] -> OType[T2, T1],
    M2 -> SetType[S2], e2[_] -> OType[T3, T2], 
    f2[_] -> OType[T3, T2], X -> OType[T1, T1]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[compsor[E1, E2][X], compso[E2, E1][X]]
 ]
*/

        {
            "COQQ-73 comp_soErl",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T2, T1].
                Var f1 : BASIS[S1]->OTYPE[T2, T1].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T3, T2].
                Var f2 : BASIS[S2]->OTYPE[T3, T2].
                Var X : OTYPE[T1, T1].

                Def E1 := superop S1 M1 T2 T1 e1 f1.
                Def E2 := superop S2 M2 T3 T2 e2 f2.
            )",
            "compsor T3 T2 T1 E1 E2 X",
            "compso T3 T2 T1 E2 E1 X"
        },

/*
COQQ-74 id_soE

Block[
 {DiracCtx = {X -> OType[T, T]}},
 DNEqQ[
  idso[T][X],
  X
  ]
 ]
*/
    
        {
            "COQQ-74 id_soE",
            R"(
                Var T : INDEX.
                Var X : OTYPE[T, T].
            )",
            "idso T X",
            "X"
        },


/*
COQQ-75 comp_soE

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], f2[_] -> OType[T2, T3],
     X -> OType[T3, T3]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compso[E1, E2][X],
  E1[E2[X]]
  ]
 ]
*/

        {
            "COQQ-75 comp_soE",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
            )",
            "compso T1 T2 T3 E1 E2 X",
            "E1 (E2 X)"
        },

/*
COQQ-76 comp_sorE

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], f2[_] -> OType[T2, T3],
     X -> OType[T3, T3]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compsor[E2, E1][X],
  E1[E2[X]]
  ]
 ]
*/

        {
            "COQQ-76 comp_sorE",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
            )",
            "compsor T1 T2 T3 E2 E1 X",
            "E1 (E2 X)"
        },

/*
COQQ-77 comp_soA

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], 
    f2[_] -> OType[T2, T3],
    M3 -> SetType[S3], e3[_] -> OType[T3, T4], f3[_] -> OType[T3, T4],
     X -> OType[T4, T4]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E3 = superop[M3, e3, f3];
 DNEqQ[
  compso[E1, compso[E2, E3]][X],
  compso[compso[E1, E2], E3][X]
  ]
 ]
*/

        {
            "COQQ-77 comp_soA",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T3, T4].
                Var f3 : BASIS[S3]->OTYPE[T3, T4].
                Var X : OTYPE[T4, T4].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
                Def E3 := superop S3 M3 T3 T4 e3 f3.
            )",
            "compso T1 T2 T4 E1 (compso T2 T3 T4 E2 E3) X",
            "compso T1 T3 T4 (compso T1 T2 T3 E1 E2) E3 X"
        },

/*
COQQ-78 comp_sorA

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], 
    f2[_] -> OType[T2, T3],
    M3 -> SetType[S3], e3[_] -> OType[T3, T4], f3[_] -> OType[T3, T4],
     X -> OType[T4, T4]}},
 E3 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E1 = superop[M3, e3, f3];
 DNEqQ[
  compsor[E1, compsor[E2, E3]][X],
  compsor[compsor[E1, E2], E3][X]
  ]
 ]
*/

        {
            "COQQ-78 comp_sorA",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T3, T4].
                Var f3 : BASIS[S3]->OTYPE[T3, T4].
                Var X : OTYPE[T4, T4].

                Def E3 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
                Def E1 := superop S3 M3 T3 T4 e3 f3.
            )",
            "compsor T1 T3 T4 E1 (compsor T1 T2 T3 E2 E3) X",
            "compsor T1 T2 T4 (compsor T2 T3 T4 E1 E2) E3 X"
        },

/*
COQQ-79 linear_comp_so

Block[
 {DiracCtx = {
    m1 -> SetType[M1], m2 -> SetType[M2], m3 -> SetType[M3],
    e1[_] -> OType[T1, T2], f1[_] -> OType[T1, T2],
    e2[_] -> OType[T2, T3], f2[_] -> OType[T2, T3],
    e3[_] -> OType[T2, T3], f3[_] -> OType[T2, T3], 
    X -> OType[T3, T3], a -> SType}},
 E1 = superop[m1, e1, f1]; E2 = superop[m2, e2, f2]; 
 E3 = superop[m3, e3, f3];
 DNEqQ[
  compso[E1, addso[scaleso[a, E2], E3]][X],
  addso[scaleso[a, compso[E1, E2]], compso[E1, E3]][X]
  ]
 ]
*/

        {
            "COQQ-79 linear_comp_so",
            R"(
                Var M1 : INDEX.
                Var M2 : INDEX.
                Var M3 : INDEX.
                Var m1 : SET[M1].
                Var m2 : SET[M2].
                Var m3 : SET[M3].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var e1 : BASIS[M1]->OTYPE[T1, T2].
                Var f1 : BASIS[M1]->OTYPE[T1, T2].
                Var e2 : BASIS[M2]->OTYPE[T2, T3].
                Var f2 : BASIS[M2]->OTYPE[T2, T3].
                Var e3 : BASIS[M3]->OTYPE[T2, T3].
                Var f3 : BASIS[M3]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].
                Var a : STYPE.

                Def E1 := superop M1 m1 T1 T2 e1 f1.
                Def E2 := superop M2 m2 T2 T3 e2 f2.
                Def E3 := superop M3 m3 T2 T3 e3 f3.
            )",
            "compso T1 T2 T3 E1 (addso T2 T3 (scaleso T2 T3 a E2) E3) X",
            "addso T1 T3 (scaleso T1 T3 a (compso T1 T2 T3 E1 E2)) (compso T1 T2 T3 E1 E3) X"
        },

/*
COQQ-80 linear_compr_so

Block[
 {DiracCtx = {
    m1 -> SetType[M1], m2 -> SetType[M2], m3 -> SetType[M3],
    e1[_] -> OType[T1, T2], f1[_] -> OType[T1, T2],
    e2[_] -> OType[T1, T2], f2[_] -> OType[T1, T2],
    e3[_] -> OType[T2, T3], f3[_] -> OType[T2, T3], 
    X -> OType[T3, T3], a -> SType}},
 E1 = superop[m1, e1, f1]; E2 = superop[m2, e2, f2]; 
 E3 = superop[m3, e3, f3];
 DNEqQ[
  compso[addso[scaleso[a, E1], E2], E3][X],
  addso[scaleso[a, compso[E1, E3]], compso[E2, E3]][X]
  ]
 ]
*/

        {
            "COQQ-80 linear_compr_so",
            R"(
                Var M1 : INDEX.
                Var M2 : INDEX.
                Var M3 : INDEX.
                Var m1 : SET[M1].
                Var m2 : SET[M2].
                Var m3 : SET[M3].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var e1 : BASIS[M1]->OTYPE[T1, T2].
                Var f1 : BASIS[M1]->OTYPE[T1, T2].
                Var e2 : BASIS[M2]->OTYPE[T1, T2].
                Var f2 : BASIS[M2]->OTYPE[T1, T2].
                Var e3 : BASIS[M3]->OTYPE[T2, T3].
                Var f3 : BASIS[M3]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].
                Var a : STYPE.

                Def E1 := superop M1 m1 T1 T2 e1 f1.
                Def E2 := superop M2 m2 T1 T2 e2 f2.
                Def E3 := superop M3 m3 T2 T3 e3 f3.
            )",
            "compso T1 T2 T3 (addso T1 T2 (scaleso T1 T2 a E1) E2) E3 X",
            "addso T1 T3 (scaleso T1 T3 a (compso T1 T2 T3 E1 E3)) (compso T1 T2 T3 E2 E3) X"
        },

/*
COQQ-81 linear_comp_sor

Block[
 {DiracCtx = {
    m1 -> SetType[M1], m2 -> SetType[M2], m3 -> SetType[M3],
    e1[_] -> OType[T1, T2], f1[_] -> OType[T1, T2],
    e2[_] -> OType[T3, T1], f2[_] -> OType[T3, T1],
    e3[_] -> OType[T3, T1], f3[_] -> OType[T3, T1], 
    X -> OType[T2, T2], a -> SType}},
 E1 = superop[m1, e1, f1]; E2 = superop[m2, e2, f2]; 
 E3 = superop[m3, e3, f3];
 DNEqQ[
  compsor[E1, addso[scaleso[a, E2], E3]][X],
  addso[scaleso[a, compsor[E1, E2]], compsor[E1, E3]][X]
  ]
 ]
*/

        {
            "COQQ-81 linear_comp_sor",
            R"(
                Var M1 : INDEX.
                Var M2 : INDEX.
                Var M3 : INDEX.
                Var m1 : SET[M1].
                Var m2 : SET[M2].
                Var m3 : SET[M3].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var e1 : BASIS[M1]->OTYPE[T1, T2].
                Var f1 : BASIS[M1]->OTYPE[T1, T2].
                Var e2 : BASIS[M2]->OTYPE[T3, T1].
                Var f2 : BASIS[M2]->OTYPE[T3, T1].
                Var e3 : BASIS[M3]->OTYPE[T3, T1].
                Var f3 : BASIS[M3]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.

                Def E1 := superop M1 m1 T1 T2 e1 f1.
                Def E2 := superop M2 m2 T3 T1 e2 f2.
                Def E3 := superop M3 m3 T3 T1 e3 f3.
            )",
            "compsor T3 T1 T2 E1 (addso T3 T1 (scaleso T3 T1 a E2) E3) X",
            "addso T3 T2 (scaleso T3 T2 a (compsor T3 T1 T2 E1 E2)) (compsor T3 T1 T2 E1 E3) X"
        },

/*
COQQ-82 linear_compr_sor

Block[
 {DiracCtx = {
    m1 -> SetType[M1], m2 -> SetType[M2], m3 -> SetType[M3],
    e1[_] -> OType[T1, T2], f1[_] -> OType[T1, T2],
    e2[_] -> OType[T1, T2], f2[_] -> OType[T1, T2],
    e3[_] -> OType[T3, T1], f3[_] -> OType[T3, T1], 
    X -> OType[T2, T2], a -> SType}},
 E1 = superop[m1, e1, f1]; E2 = superop[m2, e2, f2]; 
 E3 = superop[m3, e3, f3];
 DNEqQ[
  compsor[addso[scaleso[a, E1], E2], E3][X],
  addso[scaleso[a, compsor[E1, E3]], compsor[E2, E3]][X]
  ]
 ]
*/

        {
            "COQQ-82 linear_compr_sor",
            R"(
                Var M1 : INDEX.
                Var M2 : INDEX.
                Var M3 : INDEX.
                Var m1 : SET[M1].
                Var m2 : SET[M2].
                Var m3 : SET[M3].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var e1 : BASIS[M1]->OTYPE[T1, T2].
                Var f1 : BASIS[M1]->OTYPE[T1, T2].
                Var e2 : BASIS[M2]->OTYPE[T1, T2].
                Var f2 : BASIS[M2]->OTYPE[T1, T2].
                Var e3 : BASIS[M3]->OTYPE[T3, T1].
                Var f3 : BASIS[M3]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.

                Def E1 := superop M1 m1 T1 T2 e1 f1.
                Def E2 := superop M2 m2 T1 T2 e2 f2.
                Def E3 := superop M3 m3 T3 T1 e3 f3.
            )",
            "compsor T3 T1 T2 (addso T1 T2 (scaleso T1 T2 a E1) E2) E3 X",
            "addso T3 T2 (scaleso T3 T2 a (compsor T3 T1 T2 E1 E3)) (compsor T3 T1 T2 E2 E3) X"
        },

/*
COQQ-83 comp_so1l

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  compso[idso[T1], E1][X],
  E1[X]
  ]
 ]
*/

        {
            "COQQ-83 comp_so1l",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S M T1 T2 e f.
            )",
            "compso T1 T1 T2 (idso T1) E1 X",
            "E1 X"
        },

/*
COQQ-84 comp_so1r

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  compso[E1, idso[T2]][X],
  E1[X]
  ]
 ]
*/

        {
            "COQQ-84 comp_so1r",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S M T1 T2 e f.
            )",
            "compso T1 T2 T2 E1 (idso T2) X",
            "E1 X"
        },

/*  
COQQ-85 comp_so0l

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  compso[abortso[T3, T1], E1][X],
  abortso[T3, T2][X]
  ]
 ]
*/

        {
            "COQQ-85 comp_so0l",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S M T1 T2 e f.
            )",
            "compso T3 T1 T2 (abortso T3 T1) E1 X",
            "abortso T3 T2 X"
        },

/*
COQQ-86 comp_so0r

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T3, T3]}},
 E1 = superop[M, e, f];
 DNEqQ[
  compso[E1, abortso[T2, T3]][X],
  abortso[T1, T3][X]
  ]
 ]
*/

        {
            "COQQ-86 comp_so0r",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T3, T3].

                Def E1 := superop S M T1 T2 e f.
            )",
            "compso T1 T2 T3 E1 (abortso T2 T3) X",
            "abortso T1 T3 X"
        },

/*
COQQ-87 comp_soDl

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T1, T2], 
    f2[_] -> OType[T1, T2],
    M3 -> SetType[S3], e3[_] -> OType[T2, T3], f3[_] -> OType[T2, T3],
     X -> OType[T3, T3]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E3 = superop[M3, e3, f3];
 DNEqQ[
  compso[addso[E1, E2], E3][X],
  addso[compso[E1, E3], compso[E2, E3]][X]
  ]
 ]
*/

        {
            "COQQ-87 comp_soDl",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T1, T2].
                Var f2 : BASIS[S2]->OTYPE[T1, T2].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T2, T3].
                Var f3 : BASIS[S3]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T1 T2 e2 f2.
                Def E3 := superop S3 M3 T2 T3 e3 f3.
            )",
            "compso T1 T2 T3 (addso T1 T2 E1 E2) E3 X",
            "addso T1 T3 (compso T1 T2 T3 E1 E3) (compso T1 T2 T3 E2 E3) X"
        },

/*
COQQ-88 comp_soDr

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], 
    f2[_] -> OType[T2, T3],
    M3 -> SetType[S3], e3[_] -> OType[T2, T3], f3[_] -> OType[T2, T3],
     X -> OType[T3, T3]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E3 = superop[M3, e3, f3];
 DNEqQ[
  compso[E1, addso[E2, E3]][X],
  addso[compso[E1, E2], compso[E1, E3]][X]
  ]
 ]
*/

        {
            "COQQ-88 comp_soDr",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T2, T3].
                Var f3 : BASIS[S3]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
                Def E3 := superop S3 M3 T2 T3 e3 f3.
            )",
            "compso T1 T2 T3 E1 (addso T2 T3 E2 E3) X",
            "addso T1 T3 (compso T1 T2 T3 E1 E2) (compso T1 T2 T3 E1 E3) X"
        },

/*
COQQ-89 comp_soNl

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], f2[_] -> OType[T2, T3],
     X -> OType[T3, T3]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compso[oppso[E1], E2][X],
  oppso[compso[E1, E2]][X]
  ]
 ]
*/

        // {
        //     "COQQ-89 comp_soNl",
        //     R"(
        //         Var S1 : INDEX.
        //         Var S2 : INDEX.
        //         Var T1 : INDEX.
        //         Var T2 : INDEX.
        //         Var T3 : INDEX.
        //         Var M1 : SET[S1].
        //         Var e1 : BASIS[S1]->OTYPE[T1, T2].
        //         Var f1 : BASIS[S1]->OTYPE[T1, T2].
        //         Var M2 : SET[S2].
        //         Var e2 : BASIS[S2]->OTYPE[T2, T3].
        //         Var f2 : BASIS[S2]->OTYPE[T2, T3].
        //         Var X : OTYPE[T3, T3].

        //         Def E1 := superop S1 M1 T1 T2 e1 f1.
        //         Def E2 := superop S2 M2 T2 T3 e2 f2.
        //     )",
        //     "compso T1 T2 T3 (oppso T1 T2 E1) E2 X",
        //     "oppso T1 T3 (compso T1 T2 T3 E1 E2) X"
        // },


/*
COQQ-90 comp_soNr

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], f2[_] -> OType[T2, T3],
     X -> OType[T3, T3]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compso[E1, oppso[E2]][X],
  oppso[compso[E1, E2]][X]
  ]
 ]
*/

        // {
        //     "COQQ-90 comp_soNr",
        //     R"(
        //         Var S1 : INDEX.
        //         Var S2 : INDEX.
        //         Var T1 : INDEX.
        //         Var T2 : INDEX.
        //         Var T3 : INDEX.
        //         Var M1 : SET[S1].
        //         Var e1 : BASIS[S1]->OTYPE[T1, T2].
        //         Var f1 : BASIS[S1]->OTYPE[T1, T2].
        //         Var M2 : SET[S2].
        //         Var e2 : BASIS[S2]->OTYPE[T2, T3].
        //         Var f2 : BASIS[S2]->OTYPE[T2, T3].
        //         Var X : OTYPE[T3, T3].

        //         Def E1 := superop S1 M1 T1 T2 e1 f1.
        //         Def E2 := superop S2 M2 T2 T3 e2 f2.
        //     )",
        //     "compso T1 T2 T3 E1 (oppso T2 T3 E2) X",
        //     "oppso T1 T3 (compso T1 T2 T3 E1 E2) X"
        // },

/*
COQQ-91 comp_soZl

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], f2[_] -> OType[T2, T3],
     X -> OType[T3, T3], a -> SType}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compso[scaleso[a, E1], E2][X],
  scaleso[a, compso[E1, E2]][X]
  ]
 ]
*/

        {
            "COQQ-91 comp_soZl",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].
                Var a : STYPE.

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
            )",
            "compso T1 T2 T3 (scaleso T1 T2 a E1) E2 X",
            "scaleso T1 T3 a (compso T1 T2 T3 E1 E2) X"
        },

/*
COQQ-92 comp_soZr

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], f2[_] -> OType[T2, T3],
     X -> OType[T3, T3], a -> SType}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compso[E1, scaleso[a, E2]][X],
  scaleso[a, compso[E1, E2]][X]
  ]
 ]
*/
        
        {
            "COQQ-92 comp_soZr",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].
                Var a : STYPE.

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
            )",
            "compso T1 T2 T3 E1 (scaleso T2 T3 a E2) X",
            "scaleso T1 T3 a (compso T1 T2 T3 E1 E2) X"
        },

/*
COQQ-93 comp_soPl

Block[
 {DiracCtx = {
    m1 -> SetType[M1], m2 -> SetType[M2], m3 -> SetType[M3],
    e1[_] -> OType[T1, T2], f1[_] -> OType[T1, T2],
    e2[_] -> OType[T1, T2], f2[_] -> OType[T1, T2],
    e3[_] -> OType[T2, T3], f3[_] -> OType[T2, T3], 
    X -> OType[T3, T3], a -> SType}},
 E1 = superop[m1, e1, f1]; E2 = superop[m2, e2, f2]; 
 E3 = superop[m3, e3, f3];
 DNEqQ[
  compso[addso[scaleso[a, E1], E2], E3][X],
  addso[scaleso[a, compso[E1, E3]], compso[E2, E3]][X]
  ]
 ]
*/

        {
            "COQQ-93 comp_soPl",
            R"(
                Var M1 : INDEX.
                Var M2 : INDEX.
                Var M3 : INDEX.
                Var m1 : SET[M1].
                Var m2 : SET[M2].
                Var m3 : SET[M3].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var e1 : BASIS[M1]->OTYPE[T1, T2].
                Var f1 : BASIS[M1]->OTYPE[T1, T2].
                Var e2 : BASIS[M2]->OTYPE[T1, T2].
                Var f2 : BASIS[M2]->OTYPE[T1, T2].
                Var e3 : BASIS[M3]->OTYPE[T2, T3].
                Var f3 : BASIS[M3]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].
                Var a : STYPE.

                Def E1 := superop M1 m1 T1 T2 e1 f1.
                Def E2 := superop M2 m2 T1 T2 e2 f2.
                Def E3 := superop M3 m3 T2 T3 e3 f3.
            )",
            "compso T1 T2 T3 (addso T1 T2 (scaleso T1 T2 a E1) E2) E3 X",
            "addso T1 T3 (scaleso T1 T3 a (compso T1 T2 T3 E1 E3)) (compso T1 T2 T3 E2 E3) X"
        },

/*
COQQ-94 comp_soPr

Block[
 {DiracCtx = {
    m1 -> SetType[M1], m2 -> SetType[M2], m3 -> SetType[M3],
    e1[_] -> OType[T1, T2], f1[_] -> OType[T1, T2],
    e2[_] -> OType[T2, T3], f2[_] -> OType[T2, T3],
    e3[_] -> OType[T2, T3], f3[_] -> OType[T2, T3], 
    X -> OType[T3, T3], a -> SType}},
 E1 = superop[m1, e1, f1]; E2 = superop[m2, e2, f2]; 
 E3 = superop[m3, e3, f3];
 DNEqQ[
  compso[E1, addso[scaleso[a, E2], E3]][X],
  addso[scaleso[a, compso[E1, E2]], compso[E1, E3]][X]
  ]
 ]
*/

        {
            "COQQ-94 comp_soPr",
            R"(
                Var M1 : INDEX.
                Var M2 : INDEX.
                Var M3 : INDEX.
                Var m1 : SET[M1].
                Var m2 : SET[M2].
                Var m3 : SET[M3].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var e1 : BASIS[M1]->OTYPE[T1, T2].
                Var f1 : BASIS[M1]->OTYPE[T1, T2].
                Var e2 : BASIS[M2]->OTYPE[T2, T3].
                Var f2 : BASIS[M2]->OTYPE[T2, T3].
                Var e3 : BASIS[M3]->OTYPE[T2, T3].
                Var f3 : BASIS[M3]->OTYPE[T2, T3].
                Var X : OTYPE[T3, T3].
                Var a : STYPE.

                Def E1 := superop M1 m1 T1 T2 e1 f1.
                Def E2 := superop M2 m2 T2 T3 e2 f2.
                Def E3 := superop M3 m3 T2 T3 e3 f3.
            )",
            "compso T1 T2 T3 E1 (addso T2 T3 (scaleso T2 T3 a E2) E3) X",
            "addso T1 T3 (scaleso T1 T3 a (compso T1 T2 T3 E1 E2)) (compso T1 T2 T3 E1 E3) X"
        },

/*
COQQ-95 comp_sor1l

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  compsor[idso[T2], E1][X],
  E1[X]
  ]
 ]
 */

        {
            "COQQ-95 comp_sor1l",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S M T1 T2 e f.
            )",
            "compsor T1 T2 T2 (idso T2) E1 X",
            "E1 X"
        },

/*
COQQ-96 comp_sor1r

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  compsor[E1, idso[T1]][X],
  E1[X]
  ]
 ]
*/

        {
            "COQQ-96 comp_sor1r",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S M T1 T2 e f.
            )",
            "compsor T1 T1 T2 E1 (idso T1) X",
            "E1 X"
        },

/*
COQQ-97 comp_sor0l

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T3, T3]}},
 E1 = superop[M, e, f];
 DNEqQ[
  compsor[abortso[T2, T3], E1][X],
  abortso[T1, T3][X]
  ]
 ]
*/

        {
            "COQQ-97 comp_sor0l",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T3, T3].

                Def E1 := superop S M T1 T2 e f.
            )",
            "compsor T1 T2 T3 (abortso T2 T3) E1 X",
            "abortso T1 T3 X"
        },

/*
COQQ-98 comp_sor0r

Block[
 {DiracCtx = {M -> SetType[S], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], X -> OType[T2, T2]}},
 E1 = superop[M, e, f];
 DNEqQ[
  compsor[E1, abortso[T3, T1]][X],
  abortso[T3, T2][X]
  ]
 ]
*/

        {
            "COQQ-98 comp_sor0r",
            R"(
                Var S : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M : SET[S].
                Var e : BASIS[S]->OTYPE[T1, T2].
                Var f : BASIS[S]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S M T1 T2 e f.
            )",
            "compsor T3 T1 T2 E1 (abortso T3 T1) X",
            "abortso T3 T2 X"
        },

/*
COQQ-99 comp_sorDl

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T1, T2], 
    f2[_] -> OType[T1, T2],
    M3 -> SetType[S3], e3[_] -> OType[T3, T1], f3[_] -> OType[T3, T1],
     X -> OType[T2, T2]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E3 = superop[M3, e3, f3];
 DNEqQ[
  compsor[addso[E1, E2], E3][X],
  addso[compsor[E1, E3], compsor[E2, E3]][X]
  ]
 ]
*/

        {
            "COQQ-99 comp_sorDl",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T1, T2].
                Var f2 : BASIS[S2]->OTYPE[T1, T2].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T3, T1].
                Var f3 : BASIS[S3]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T1 T2 e2 f2.
                Def E3 := superop S3 M3 T3 T1 e3 f3.
            )",
            "compsor T3 T1 T2 (addso T1 T2 E1 E2) E3 X",
            "addso T3 T2 (compsor T3 T1 T2 E1 E3) (compsor T3 T1 T2 E2 E3) X"
        },

/*
COQQ-100 comp_sorDr

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T3, T1], 
    f2[_] -> OType[T3, T1],
    M3 -> SetType[S3], e3[_] -> OType[T3, T1], f3[_] -> OType[T3, T1],
     X -> OType[T2, T2]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E3 = superop[M3, e3, f3];
 DNEqQ[
  compsor[E1, addso[E2, E3]][X],
  addso[compsor[E1, E2], compsor[E1, E3]][X]
  ]
 ]
*/

        {
            "COQQ-100 comp_sorDr",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T3, T1].
                Var f2 : BASIS[S2]->OTYPE[T3, T1].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T3, T1].
                Var f3 : BASIS[S3]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T3 T1 e2 f2.
                Def E3 := superop S3 M3 T3 T1 e3 f3.
            )",
            "compsor T3 T1 T2 E1 (addso T3 T1 E2 E3) X",
            "addso T3 T2 (compsor T3 T1 T2 E1 E2) (compsor T3 T1 T2 E1 E3) X"
        },

/*
COQQ-101 comp_sorNl

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T3, T1], f2[_] -> OType[T3, T1],
     X -> OType[T2, T2]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compsor[oppso[E1], E2][X],
  oppso[compsor[E1, E2]][X]
  ]
 ]
*/

        // {
        //     "COQQ-101 comp_sorNl",
        //     R"(
        //         Var S1 : INDEX.
        //         Var S2 : INDEX.
        //         Var T1 : INDEX.
        //         Var T2 : INDEX.
        //         Var T3 : INDEX.
        //         Var M1 : SET[S1].
        //         Var e1 : BASIS[S1]->OTYPE[T1, T2].
        //         Var f1 : BASIS[S1]->OTYPE[T1, T2].
        //         Var M2 : SET[S2].
        //         Var e2 : BASIS[S2]->OTYPE[T3, T1].
        //         Var f2 : BASIS[S2]->OTYPE[T3, T1].
        //         Var X : OTYPE[T2, T2].

        //         Def E1 := superop S1 M1 T1 T2 e1 f1.
        //         Def E2 := superop S2 M2 T3 T1 e2 f2.
        //     )",
        //     "compsor T1 T2 T2 (oppso T1 T2 E1) E2 X",
        //     "oppso T1 T2 (compsor T1 T2 T2 E1 E2) X"
        // },

/*
COQQ-102 comp_sorNr

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T3, T1], f2[_] -> OType[T3, T1],
     X -> OType[T2, T2]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compsor[E1, oppso[E2]][X],
  oppso[compsor[E1, E2]][X]
  ]
 ]
*/

        // {
        //     "COQQ-102 comp_sorNr",
        //     R"(
        //         Var S1 : INDEX.
        //         Var S2 : INDEX.
        //         Var T1 : INDEX.
        //         Var T2 : INDEX.
        //         Var T3 : INDEX.
        //         Var M1 : SET[S1].
        //         Var e1 : BASIS[S1]->OTYPE[T1, T2].
        //         Var f1 : BASIS[S1]->OTYPE[T1, T2].
        //         Var M2 : SET[S2].
        //         Var e2 : BASIS[S2]->OTYPE[T3, T1].
        //         Var f2 : BASIS[S2]->OTYPE[T3, T1].
        //         Var X : OTYPE[T2, T2].

        //         Def E1 := superop S1 M1 T1 T2 e1 f1.
        //         Def E2 := superop S2 M2 T3 T1 e2 f2.
        //     )",
        //     "compsor T1 T2 T2 E1 (oppso T3 T1 E2) X",
        //     "oppso T1 T2 (compsor T1 T2 T2 E1 E2) X"
        // },

/*
COQQ-103 comp_sorZl

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T3, T1], f2[_] -> OType[T3, T1],
     X -> OType[T2, T2], a -> SType}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compsor[scaleso[a, E1], E2][X],
  scaleso[a, compsor[E1, E2]][X]
  ]
 ]
*/

        {
            "COQQ-103 comp_sorZl",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T3, T1].
                Var f2 : BASIS[S2]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T3 T1 e2 f2.
            )",
            "compsor T3 T1 T2 (scaleso T1 T2 a E1) E2 X",
            "scaleso T3 T2 a (compsor T3 T1 T2 E1 E2) X"
        },

/*
COQQ-104 comp_sorZr

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T3, T1], f2[_] -> OType[T3, T1],
     X -> OType[T2, T2], a -> SType}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[
  compsor[E1, scaleso[a, E2]][X],
  scaleso[a, compsor[E1, E2]][X]
  ]
 ]
*/

        {
            "COQQ-104 comp_sorZr",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T3, T1].
                Var f2 : BASIS[S2]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T3 T1 e2 f2.
            )",
            "compsor T3 T1 T2 E1 (scaleso T3 T1 a E2) X",
            "scaleso T3 T2 a (compsor T3 T1 T2 E1 E2) X"
        },

/*
COQQ-105 comp_sorPl

Block[
 {DiracCtx = {
    m1 -> SetType[M1], m2 -> SetType[M2], m3 -> SetType[M3],
    e1[_] -> OType[T1, T2], f1[_] -> OType[T1, T2],
    e2[_] -> OType[T1, T2], f2[_] -> OType[T1, T2],
    e3[_] -> OType[T3, T1], f3[_] -> OType[T3, T1], 
    X -> OType[T2, T2], a -> SType}},
 E1 = superop[m1, e1, f1]; E2 = superop[m2, e2, f2]; 
 E3 = superop[m3, e3, f3];
 DNEqQ[
  compsor[addso[scaleso[a, E1], E2], E3][X],
  addso[scaleso[a, compsor[E1, E3]], compsor[E2, E3]][X]
  ]
 ]
*/

        {
            "COQQ-105 comp_sorPl",
            R"(
                Var M1 : INDEX.
                Var M2 : INDEX.
                Var M3 : INDEX.
                Var m1 : SET[M1].
                Var m2 : SET[M2].
                Var m3 : SET[M3].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var e1 : BASIS[M1]->OTYPE[T1, T2].
                Var f1 : BASIS[M1]->OTYPE[T1, T2].
                Var e2 : BASIS[M2]->OTYPE[T1, T2].
                Var f2 : BASIS[M2]->OTYPE[T1, T2].
                Var e3 : BASIS[M3]->OTYPE[T3, T1].
                Var f3 : BASIS[M3]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.

                Def E1 := superop M1 m1 T1 T2 e1 f1.
                Def E2 := superop M2 m2 T1 T2 e2 f2.
                Def E3 := superop M3 m3 T3 T1 e3 f3.
            )",
            "compsor T3 T1 T2 (addso T1 T2 (scaleso T1 T2 a E1) E2) E3 X",
            "addso T3 T2 (scaleso T3 T2 a (compsor T3 T1 T2 E1 E3)) (compsor T3 T1 T2 E2 E3) X"
        },

/*
COQQ-106 comp_sorPr

Block[
 {DiracCtx = {
    m1 -> SetType[M1], m2 -> SetType[M2], m3 -> SetType[M3],
    e1[_] -> OType[T1, T2], f1[_] -> OType[T1, T2],
    e2[_] -> OType[T3, T1], f2[_] -> OType[T3, T1],
    e3[_] -> OType[T3, T1], f3[_] -> OType[T3, T1], 
    X -> OType[T2, T2], a -> SType}},
 E1 = superop[m1, e1, f1]; E2 = superop[m2, e2, f2]; 
 E3 = superop[m3, e3, f3];
 DNEqQ[
  compsor[E1, addso[scaleso[a, E2], E3]][X],
  addso[scaleso[a, compsor[E1, E2]], compsor[E1, E3]][X]
  ]
 ]
*/

        {
            "COQQ-106 comp_sorPr",
            R"(
                Var M1 : INDEX.
                Var M2 : INDEX.
                Var M3 : INDEX.
                Var m1 : SET[M1].
                Var m2 : SET[M2].
                Var m3 : SET[M3].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var e1 : BASIS[M1]->OTYPE[T1, T2].
                Var f1 : BASIS[M1]->OTYPE[T1, T2].
                Var e2 : BASIS[M2]->OTYPE[T3, T1].
                Var f2 : BASIS[M2]->OTYPE[T3, T1].
                Var e3 : BASIS[M3]->OTYPE[T3, T1].
                Var f3 : BASIS[M3]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].
                Var a : STYPE.

                Def E1 := superop M1 m1 T1 T2 e1 f1.
                Def E2 := superop M2 m2 T3 T1 e2 f2.
                Def E3 := superop M3 m3 T3 T1 e3 f3.
            )",
            "compsor T3 T1 T2 E1 (addso T3 T1 (scaleso T3 T1 a E2) E3) X",
            "addso T3 T2 (scaleso T3 T2 a (compsor T3 T1 T2 E1 E2)) (compsor T3 T1 T2 E1 E3) X"
        },

/*
COQQ-107 comp_soACA

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T1, T2], 
    f1[_] -> OType[T1, T2],
    M2 -> SetType[S2], e2[_] -> OType[T2, T3], 
    f2[_] -> OType[T2, T3],
    M3 -> SetType[S3], e3[_] -> OType[T3, T4], 
    f3[_] -> OType[T3, T4],
    M4 -> SetType[S4], e4[_] -> OType[T4, T5], f4[_] -> OType[T4, T5],
     X -> OType[T5, T5]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E3 = superop[M3, e3, f3]; E4 = superop[M4, e4, f4];
 DNEqQ[
  compso[compso[compso[E1, E2], E3], E4][X],
  compso[compso[E1, compso[E2, E3]], E4][X]
  ]
 ]
*/

        {
            "COQQ-107 comp_soACA",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var S4 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var T5 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T1, T2].
                Var f1 : BASIS[S1]->OTYPE[T1, T2].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T2, T3].
                Var f2 : BASIS[S2]->OTYPE[T2, T3].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T3, T4].
                Var f3 : BASIS[S3]->OTYPE[T3, T4].
                Var M4 : SET[S4].
                Var e4 : BASIS[S4]->OTYPE[T4, T5].
                Var f4 : BASIS[S4]->OTYPE[T4, T5].
                Var X : OTYPE[T5, T5].

                Def E1 := superop S1 M1 T1 T2 e1 f1.
                Def E2 := superop S2 M2 T2 T3 e2 f2.
                Def E3 := superop S3 M3 T3 T4 e3 f3.
                Def E4 := superop S4 M4 T4 T5 e4 f4.
            )",
            "compso T1 T3 T5 (compso T1 T2 T3 E1 E2) (compso T3 T4 T5 E3 E4) X",
            "compso T1 T4 T5 (compso T1 T2 T4 E1 (compso T2 T3 T4 E2 E3)) E4 X"
        },


/*
COQQ-108 comp_sorACA

Block[
 {DiracCtx = {M1 -> SetType[S1], e1[_] -> OType[T2, T1], 
    f1[_] -> OType[T2, T1],
    M2 -> SetType[S2], e2[_] -> OType[T3, T2], 
    f2[_] -> OType[T3, T2],
    M3 -> SetType[S3], e3[_] -> OType[T4, T3], 
    f3[_] -> OType[T4, T3],
    M4 -> SetType[S4], e4[_] -> OType[T5, T4], f4[_] -> OType[T5, T4],
     X -> OType[T1, T1]}},
   E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2]; 
 E3 = superop[M3, e3, f3]; E4 = superop[M4, e4, f4];
 DNEqQ[
  compsor[compsor[compsor[E1, E2], E3], E4][X],
  compsor[compsor[E1, compsor[E2, E3]], E4][X]
  ]
 ]
*/

        {
            "COQQ-108 comp_sorACA",
            R"(
                Var S1 : INDEX.
                Var S2 : INDEX.
                Var S3 : INDEX.
                Var S4 : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var T4 : INDEX.
                Var T5 : INDEX.
                Var M1 : SET[S1].
                Var e1 : BASIS[S1]->OTYPE[T2, T1].
                Var f1 : BASIS[S1]->OTYPE[T2, T1].
                Var M2 : SET[S2].
                Var e2 : BASIS[S2]->OTYPE[T3, T2].
                Var f2 : BASIS[S2]->OTYPE[T3, T2].
                Var M3 : SET[S3].
                Var e3 : BASIS[S3]->OTYPE[T4, T3].
                Var f3 : BASIS[S3]->OTYPE[T4, T3].
                Var M4 : SET[S4].
                Var e4 : BASIS[S4]->OTYPE[T5, T4].
                Var f4 : BASIS[S4]->OTYPE[T5, T4].
                Var X : OTYPE[T1, T1].

                Def E1 := superop S1 M1 T2 T1 e1 f1.
                Def E2 := superop S2 M2 T3 T2 e2 f2.
                Def E3 := superop S3 M3 T4 T3 e3 f3.
                Def E4 := superop S4 M4 T5 T4 e4 f4.
            )",
            "compsor T5 T4 T1 (compsor T4 T3 T1 (compsor T3 T2 T1 E1 E2) E3) E4 X",
            "compsor T5 T4 T1 (compsor T4 T2 T1 E1 (compsor T4 T3 T2 E2 E3)) E4 X"
        },

/*
COQQ-109 krausso_fun_is_linear

Block[
 {DiracCtx = {m -> SetType[M], e[_] -> OType[T1, T2], a -> SType, 
    X -> OType[T2, T2], Y -> OType[T2, T2]}},
 DNEqQ[
  krausso[m, e][(a~SCRO~X)~ADDO~Y],
  (a~SCRO~krausso[m, e][X])~ADDO~krausso[m, e][Y]
  ]
 ]
*/
    
        {
            "COQQ-109 krausso_fun_is_linear",
            R"(
                Var M : INDEX.
                Var m : SET[M].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var e : BASIS[M]->OTYPE[T1, T2].
                Var a : STYPE.
                Var X : OTYPE[T2, T2].
                Var Y : OTYPE[T2, T2].

            )",
            "krausso M m T1 T2 e (a.X + Y)",
            "a . (krausso M m T1 T2 e X) + krausso M m T1 T2 e Y"
        },

/*
COQQ-110 kraussoE

Block[
 {DiracCtx = {m -> SetType[M], e[_] -> OType[T1, T2], 
    X -> OType[T2, T2]}},
 DNEqQ[
  krausso[m, e][X],
  Sum[e[i]\[SmallCircle]X\[SmallCircle]SuperDagger[e[i]], {i, m}]
  ]
 ]
*/

        {
            "COQQ-110 kraussoE",
            R"(
                Var M : INDEX.
                Var m : SET[M].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var e : BASIS[M]->OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

            )",
            "krausso M m T1 T2 e X",
            "Sum i in m, (e i) X (e i)^D"
        },

/*
COQQ-111 formsoE

Block[
 {DiracCtx = {A -> OType[T1, T2], X -> OType[T2, T2]}},
 DNEqQ[
  formso[A][X],
  A\[SmallCircle]X\[SmallCircle]SuperDagger[A]
  ]
 ]
*/

        {
            "COQQ-111 formsoE",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var A : OTYPE[T1, T2].
                Var X : OTYPE[T2, T2].

            )",
            "formso T1 T2 A X",
            "A X A^D"
        },

/*
COQQ-112 formso0

Block[
 {DiracCtx = {X -> OType[T2, T2]}},
 DNEqQ[
  formso[ZEROO[T1, T2]][X],
  abortso[T1, T2][X]
  ]
 ]
*/

        {
            "COQQ-112 formso0",
            R"(
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var X : OTYPE[T2, T2].

            )",
            "formso T1 T2 0O[T1, T2] X",
            "abortso T1 T2 X"
        },

/*
COQQ-113 ifso_fun_is_linear

Block[{DiracCtx = {M -> SetType[Sm], e[_] -> OType[T1, T2],
    Nm[_] -> SetType[Sn], fl[_][_] -> OType[T3, T1], 
    fr[_][_] -> OType[T3, T1],
    a -> SType, X -> OType[T2, T2], Y -> OType[T2, T2]}},
 E1 = superop[M1, e1, f1]; E2 = superop[M2, e2, f2];
 DNEqQ[ifso[M, e, superop[Nm[#], fl[#], fr[#]] &][a~SCRO~X + Y],
  a~SCRO~ifso[M, e, superop[Nm[#], fl[#], fr[#]] &][X] + 
   ifso[M, e, superop[Nm[#], fl[#], fr[#]] &][Y]]
 ]
*/

        {
            "COQQ-113 ifso_fun_is_linear",
            R"(
                Var Sm : INDEX.
                Var Sn : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M : SET[Sm].
                Var e : BASIS[Sm]->OTYPE[T1, T2].
                Var Nm : BASIS[Sm]->SET[Sn].
                Var fl : BASIS[Sm]->BASIS[Sn]->OTYPE[T3, T1].
                Var fr : BASIS[Sm]->BASIS[Sn]->OTYPE[T3, T1].
                Var a : STYPE.
                Var X : OTYPE[T2, T2].
                Var Y : OTYPE[T2, T2].

            )",
            "ifso Sm M T3 T1 T2 e (fun i : BASIS[Sm] => superop Sn (Nm i) T3 T1 (fl i) (fr i)) (a.X + Y)",
            "a . (ifso Sm M T3 T1 T2 e (fun i : BASIS[Sm] => superop Sn (Nm i) T3 T1 (fl i) (fr i)) X) + ifso Sm M T3 T1 T2 e (fun i : BASIS[Sm] => superop Sn (Nm i) T3 T1 (fl i) (fr i)) Y"
        },

/*
COQQ-114 ifsoE

Block[
 {DiracCtx = {M -> SetType[Sm], e[_] -> OType[T1, T2],
    Nm[_] -> SetType[Sn], fl[_][_] -> OType[T3, T1], 
    fr[_][_] -> OType[T3, T1], X -> OType[T2, T2]}},
 DNEqQ[
  ifso[M, e, superop[Nm[#], fl[#], fr[#]] &][X],
  Sum[superop[Nm[i], fl[i], fr[i]][
    e[i]\[SmallCircle]X\[SmallCircle]SuperDagger[(e[i])]], {i, M}]
  ]
 ]
*/

        {
            "COQQ-114 ifsoE",
            R"(
                Var Sm : INDEX.
                Var Sn : INDEX.
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var T3 : INDEX.
                Var M : SET[Sm].
                Var e : BASIS[Sm]->OTYPE[T1, T2].
                Var Nm : BASIS[Sm]->SET[Sn].
                Var fl : BASIS[Sm]->BASIS[Sn]->OTYPE[T3, T1].
                Var fr : BASIS[Sm]->BASIS[Sn]->OTYPE[T3, T1].
                Var X : OTYPE[T2, T2].

            )",
            "ifso Sm M T3 T1 T2 e (fun i : BASIS[Sm] => superop Sn (Nm i) T3 T1 (fl i) (fr i)) X",
            "Sum i in M, superop Sn (Nm i) T3 T1 (fl i) (fr i) ((e i) X (e i)^D)"
        },

/*
COQQ-115 formso1

Block[
 {DiracCtx = {X -> OType[T, T]}},
 DNEqQ[
  formso[ONEO[T]][X],
  idso[T][X]
  ]
 ]
*/
        
        {
            "COQQ-115 formso1",
            R"(
                Var T : INDEX.
                Var X : OTYPE[T, T].

            )",
            "formso T T 1O[T] X",
            "idso T X"
        },


// This one has first/second projections, omitted

/*
COQQ-116 comp_krausso

Block[
 {DiracCtx = {X -> OType[T3, T3], m1 -> SetType[M1], 
    m2 -> SetType[M2], e1[_] -> OType[T1, T2], 
    e2[_] -> OType[T2, T3]}},
 DNEqQ[
  compso[krausso[m1, e1], krausso[m2, e2]][X],
  krausso[m1~SETPROD~m2, e1[FST[#]]~MLTO~e2[SND[#]] &][X]
  ]
 ]
*/

        // {
        //     "COQQ-116 comp_krausso",
        //     R"(
        //         Var T1 : INDEX.
        //         Var T2 : INDEX.
        //         Var T3 : INDEX.
        //         Var X : OTYPE[T3, T3].
        //         Var M1 : INDEX.
        //         Var M2 : INDEX.
        //         Var m1 : SET[M1].
        //         Var m2 : SET[M2].
        //         Var e1 : BASIS[M1]->OTYPE[T1, T2].
        //         Var e2 : BASIS[M2]->OTYPE[T2, T3].

        //     )",
        //     "compso T1 T3 T3 (krausso M1 m1 T1 T2 e1) (krausso M2 m2 T2 T3 e2) X",
        //     "krausso (M1 * M2) (fun i : M1 * M2 => e1 (FST i) * e2 (SND i)) X"
        // },

/*
COQQ-117 compr_krausso omitted : first/second projections
*/

/*
COQQ-118 ifso_krausso omitted : first/second projections
*/

/*
COQQ-119 scaleso_krausso omitted : 0 <= c condition
*/

/*
COQQ-120 choimxE

Block[
 {DiracCtx = {M -> SetType[m], e[_] -> OType[T1, T2], 
    f[_] -> OType[T1, T2], x -> OType[T2, T2]}},
 	E1 = superop[M, e, f];
 DNEqQ[
  DNPTr1[
   so2choi[E1, 
     T2]\[SmallCircle](TPO[x, T2, T2]\[CircleTimes]ONEO[T1]), T2, T1, 
   T1],
  Sum[e[k]\[SmallCircle]x\[SmallCircle]SuperDagger[f[k]], {k, M}]
  ]
 ]
*/

        {
            "COQQ-120 choimxE",
            R"(
                Var m : INDEX.
                Var M : SET[m].
                Var T1 : INDEX.
                Var T2 : INDEX.
                Var e : BASIS[m]->OTYPE[T1, T2].
                Var f : BASIS[m]->OTYPE[T1, T2].
                Var x : OTYPE[T2, T2].

                Def E1 := superop m M T1 T2 e f.
            )",
            "PTr1 T2 T1 T1 ((so2choi T2 T1 E1) ((TPO T2 T2 x) * 1O[T1]))",
            "Sum k in M, (e k) x (f k)^D"
        },

    };



};
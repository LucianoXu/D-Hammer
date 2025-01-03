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




/*
COQQ-19 sumonb_out_bool(CB)
Block[{DiracCtx = {0 -> {0, 1}, 1 -> {0, 1}}},
 DNEqQ[Ket[{0}]\[SmallCircle]Bra[{0}] + 
   Ket[{1}]\[SmallCircle]Bra[{1}], ONEO[{0, 1}]]
 ]
*/

        {
            "COQQ-19 sumonb_out_bool(CB)",
            R"(
                Var qubit : INDEX.
                Var #0 : BASIS[qubit].
                Var #1 : BASIS[qubit].
            )",
            "|#0> <#0| + |#1> <#1|",
            "1O[qubit]"
        },
    };



};
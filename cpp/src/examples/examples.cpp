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
                Def phi := idx T => Sum nv : BASIS[T] in USET[T], |(nv, nv)>.
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
            "A (Sum i : BASIS[m] in USET[m], (a i) (v i))",
            "Sum i : BASIS[m] in USET[m], (a i) (A (v i))"
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
            "v^D Sum i : BASIS[m] in USET[m], (lambda i) (w i)",
            "Sum i : BASIS[m] in USET[m], (lambda i) (v^D (w i))"
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
            "(Sum i : BASIS[m] in USET[m], (lambda i).(w i))^D v",
            "Sum i : BASIS[m] in USET[m], (lambda i)^* ((w i)^D v)"
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
            "(Sum i : BASIS[T] in USET[T], (v i).|i>)^D (Sum j : BASIS[T] in USET[T], (w j).|j>)",
            "Sum i : BASIS[T] in USET[T], (v i)^* (w i)"
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
            "(Sum i : BASIS[T] in USET[T], |i> <i|) v",
            "Sum i : BASIS[T] in USET[T], (<i| v) |i>"
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
            "Sum i : BASIS[sigma] in USET[sigma], |i> <i|",
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
            "Sum i : BASIS[sigma] in USET[sigma], |i> <i|",
            "1O[tau]",
            false
        },
    };


};
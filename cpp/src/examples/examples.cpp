#include "examples.hpp"

namespace examples {

    std::vector<EqExample> eq_examples = {
        {
            "Example 1",
            R"(
                Var sigma : INDEX.
                Var K : KTYPE(sigma).
            )",
            "(TPK sigma) ((TPB sigma) K)",
            "K"
        },

/*
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
                Var M : OTYPE(T, T).
                Def phi := idx T => Sum nv : BASIS(T) in USET(T), |(nv, nv)>.
            )",
            "(M * 1O(T)) (phi T)",
            "(1O(T) * (TPO T T M)) (phi T)"
        }
    };

};
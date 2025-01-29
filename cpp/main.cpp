

#include "ualg.hpp"
#include "dirace.hpp"


using namespace ualg;
using namespace std;
using namespace dirace;

int main(int argc, const char ** argv) {

    cout << "< Dirace Prover top level built by Yingte Xu." << endl;

    // use the Wolfram Engine on MacOS
    auto [ep, lp] = wstp::init_and_openlink(wstp::MACOS_ARGC, wstp::MACOS_ARGV);

    cout << "WSTP link: " << lp << endl;
    
    auto prover = make_unique<Prover>(std_prover(lp));

    // put the code for debugging here
    prover->process(
        R"( 
        Var T : INDEX.
        Var A : OTYPE[T * T, T * T].
        Var B : OTYPE[T * T, T * T].
        Var C : OTYPE[T * T, T * T].
        Var D : OTYPE[T * T, T * T].
        Var E : OTYPE[T * T, T * T].
        Var F : OTYPE[T * T, T * T].
        Var G : OTYPE[T * T, T * T].
        Var H : OTYPE[T * T, T * T].
        Var a : REG[T]. Var b : REG[T]. Var c : REG[T]. 
        Var d : REG[T]. Var e : REG[T]. Var f : REG[T].

        CheckEq (A_(a,b);(a,b) B_(c,d);(c,d) C_(e,f);(e,f)) (D_(b,c);(b,c) E_(d,e);(d,e)) (F_(a,b);(a,b) G_(c,d);(c,d) H_(e,f);(e,f)) with (A_(a,b);(a,b) C_(e,f);(e,f)) (B_(c,d);(c,d) D_(b,c);(b,c)) (E_(d,e);(d,e) G_(c,d);(c,d)) (F_(a,b);(a,b) H_(e,f);(e,f)).
        )");

    return 0;
}
/*

SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SCR[Times[DOT[BRA[PAIR[PAIR[$32, PAIR[$31, $30]], $29]], MULK[ADJ[V], KET[PAIR[PAIR[$28, PAIR[$27, $26]], $25]]]], DOT[BRA[PAIR[PAIR[$24, PAIR[$23, $22]], $21]], MULK[V, KET[PAIR[PAIR[$20, PAIR[$19, $18]], $17]]]], DOT[BRA[PAIR[PAIR[$19, $18], PAIR[$16, $17]]], psi], DOT[BRA[PAIR[$15, PAIR[$14, $13]]], MULK[ADJ[U], KET[PAIR[$12, PAIR[$11, $10]]]]], DOT[BRA[PAIR[$9, PAIR[$8, $7]]], MULK[U, KET[PAIR[s, PAIR[$6, $23]]]]], DOT[BRA[PAIR[$6, $20]], phi], DOT[ADJ[phi], KET[PAIR[$5, $4]]], DOT[ADJ[psi], KET[PAIR[PAIR[$3, $2], PAIR[$1, $0]]]]], LDOT[LTSR[LKET[$16, c], LKET[$8, a], LDOT[LTSR[ADJ[LKET[s, r]], ADJ[LKET[$1, c]], ADJ[LKET[$5, a]], ADJ[LKET[$3, b]], ADJ[LKET[$4, ap]], ADJ[LKET[$2, bp]], ADJ[LKET[$0, cp]]], LDOT[LTSR[ADJ[LKET[$27, b]], ADJ[LKET[$28, ap]], ADJ[LKET[$26, bp]], ADJ[LKET[$25, cp]], ADJ[LBRA[$31, b]], ADJ[LBRA[$32, ap]], ADJ[LBRA[$30, bp]], ADJ[LBRA[$29, cp]]], LTSR[ADJ[LKET[$12, r]], ADJ[LKET[$11, a]], ADJ[LKET[$10, b]], ADJ[LBRA[$15, r]], ADJ[LBRA[$14, a]], ADJ[LBRA[$13, b]]]]]], LTSR[LKET[$9, r], LKET[$7, b], LKET[$24, ap], LKET[$22, bp], LKET[$21, cp]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SUM[USET[T], FUN[BASIS[T], SCR[Times[DOT[BRA[PAIR[PAIR[$20, PAIR[$19, $18]], $17]], MULK[ADJ[V], KET[PAIR[PAIR[$16, PAIR[$15, $14]], $13]]]], DOT[BRA[PAIR[PAIR[$16, PAIR[$12, $14]], $13]], MULK[V, KET[PAIR[PAIR[$11, PAIR[$10, $9]], $8]]]], DOT[BRA[PAIR[PAIR[$10, $9], PAIR[$7, $8]]], psi], DOT[BRA[PAIR[s, PAIR[$6, $15]]], MULK[ADJ[U], KET[PAIR[$5, PAIR[$4, $3]]]]], DOT[BRA[PAIR[$5, PAIR[$2, $3]]], MULK[U, KET[PAIR[s, PAIR[$1, $12]]]]], DOT[BRA[PAIR[$1, $11]], phi], DOT[ADJ[phi], KET[PAIR[$6, $20]]], DOT[ADJ[psi], KET[PAIR[PAIR[$19, $18], PAIR[$0, $17]]]]], LTSR[LKET[$7, c], LKET[$2, a], LBRA[$0, c], LBRA[$4, a]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
*/
#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqParsing, Basics1) {
    
    Kernel kernel;

    auto& sig = kernel.get_sig();

    auto actual_res = kernel.parse("FUN[x, y, APPLY[z, x]]");
    
    auto expected_res = create_term(
        sig.get_repr("FUN"), {
            create_term(sig.get_repr("x")),
            create_term(sig.get_repr("y")),
            create_term(sig.get_repr("APPLY"), {
                create_term(sig.get_repr("z")),
                create_term(sig.get_repr("x"))
            })
        }
    );

    EXPECT_EQ(*actual_res, *expected_res);
}


TEST(DiracoqParsing, Basics2) {
    
    Kernel kernel;

    EXPECT_EQ(kernel.term_to_string(kernel.parse("TYPE")), "TYPE");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("TYPE[TYPE]")), "TYPE[TYPE]");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("FORALL[x, y, APPLY[z, x]]")), "FORALL[x, y, APPLY[z, x]]");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("FUN[x, y, APPLY[z, x]]")), "FUN[x, y, APPLY[z, x]]");

}

TEST(DiracoqTypeCalc, assum) {
    Kernel kernel;

    // (Assum)
    kernel.assum(kernel.register_symbol("x"), kernel.parse("TYPE"));
    EXPECT_EQ(*kernel.calc_type(kernel.parse("x")), *kernel.parse("TYPE"));
}

TEST(DiracoqTypeCalc, def_fun) {
    Kernel kernel;

    // (DEF)
    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    kernel.def(kernel.register_symbol("f"), kernel.parse("FUN[x, T, x]"), kernel.parse("ARROW[T, T]"));
    EXPECT_EQ(*kernel.calc_type(kernel.parse("f")), *kernel.parse("ARROW[T, T]"));
}

//////////////////////////////////////////////////////
// Typing Test


// preprocessing symbols

TEST(DiracoqTypeCheck, COMPO_SS) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[a, b]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, COMPO_SK) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[a, K]"), kernel.parse("KTYPE[T]")));
}

TEST(DiracoqTypeCheck, COMPO_SB) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[a, B]"), kernel.parse("BTYPE[T]")));
}

TEST(DiracoqTypeCheck, COMPO_SO) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[a, O]"), kernel.parse("OTYPE[T1, T2]")));
}

TEST(DiracoqTypeCheck, COMPO_KS) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[K, a]"), kernel.parse("KTYPE[T]")));
}

TEST(DiracoqTypeCheck, COMPO_KK) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KTYPE[T1]"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KTYPE[T2]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[K1, K2]"), kernel.parse("KTYPE[PROD[T1, T2]]")));
}

TEST(DiracoqTypeCheck, COMPO_KB) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T1]"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T2]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[K, B]"), kernel.parse("OTYPE[T1, T2]")));
}

TEST(DiracoqTypeCheck, COMPO_BS) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[B, a]"), kernel.parse("BTYPE[T]")));
}

TEST(DiracoqTypeCheck, COMPO_BK) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[B, K]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, COMPO_BB) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B1"), kernel.parse("BTYPE[T1]"));
    kernel.assum(kernel.register_symbol("B2"), kernel.parse("BTYPE[T2]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[B1, B2]"), kernel.parse("BTYPE[PROD[T1, T2]]")));
}

TEST(DiracoqTypeCheck, COMPO_BO) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T1]"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[B, O]"), kernel.parse("BTYPE[T2]")));
}

TEST(DiracoqTypeCheck, COMPO_OS) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[O, a]"), kernel.parse("OTYPE[T1, T2]")));
}

TEST(DiracoqTypeCheck, COMPO_OK) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T2]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[O, K]"), kernel.parse("KTYPE[T1]")));
}

TEST(DiracoqTypeCheck, COMPO_OO) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OTYPE[T2, T3]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[O1, O2]"), kernel.parse("OTYPE[T1, T3]")));
}

TEST(DiracoqTypeCheck, COMPO_Arrow) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("ARROW[T1, T2]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("T1"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[f, a]"), kernel.parse("T2")));
}

TEST(DiracoqTypeCheck, COMPO_Forall) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("f"), kernel.parse("FORALL[x, KTYPE[x]]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("COMPO[f, a]"), kernel.parse("KTYPE[a]")));
}

TEST(DiracoqTypeCheck, STAR_SType) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("STAR[a, b]"), kernel.parse("STYPE")));
    EXPECT_TRUE(kernel.type_check(kernel.parse("STAR[a, b, a, b]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, STAR_Index) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("STAR[a, b]"), kernel.parse("INDEX")));
}

TEST(DiracoqTypeCheck, STAR_SET) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("SET[T]"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("SET[T]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("STAR[a, b]"), kernel.parse("SET[PROD[T, T]]")));
}

TEST(DiracoqTypeCheck, ADDG_SType) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("ADDG[a, b]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, ADDG_Other) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("KTYPE[T]"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("KTYPE[T]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("ADDG[a, b]"), kernel.parse("KTYPE[T]")));

    kernel.assum(kernel.register_symbol("c"), kernel.parse("BTYPE[T]"));
    kernel.assum(kernel.register_symbol("d"), kernel.parse("BTYPE[T]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("ADDG[c, d]"), kernel.parse("BTYPE[T]")));

    kernel.assum(kernel.register_symbol("e"), kernel.parse("OTYPE[T, T]"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("OTYPE[T, T]"));
    kernel.assum(kernel.register_symbol("g"), kernel.parse("OTYPE[T, T]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("ADDG[e, f, g]"), kernel.parse("OTYPE[T, T]")));
}


TEST(DiracoqTypeCheck, SSUM) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SSUM[i, USET[T], KET[i]]"), kernel.parse("KTYPE[T]")));
}


// main rules (internal symbols)


TEST(DiracoqTypeCheck, Index_Var) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("sigma"), kernel.parse("INDEX")));
}

TEST(DiracoqTypeCheck, Index_Prod) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("tau"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("PROD[sigma, tau]"), kernel.parse("INDEX")));
}

TEST(DiracoqTypeCheck, Index_Qbit) {
    Kernel kernel;
    EXPECT_TRUE(kernel.type_check(kernel.parse("QBIT"), kernel.parse("INDEX")));
}

TEST(DiracoqTypeCheck, Type_Basis0) {
    Kernel kernel;
    EXPECT_TRUE(kernel.type_check(kernel.parse("#0"), kernel.parse("BASIS[QBIT]")));
}

TEST(DiracoqTypeCheck, Type_Basis1) {
    Kernel kernel;
    EXPECT_TRUE(kernel.type_check(kernel.parse("#1"), kernel.parse("BASIS[QBIT]")));
}

TEST(DiracoqTypeCheck, Type_Arrow) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("TYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("ARROW[T1, T2]"), kernel.parse("TYPE")));
}

TEST(DiracoqTypeCheck, Type_Index) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("FORALL[x, KTYPE[x]]"), kernel.parse("TYPE")));
}

TEST(DiracoqTypeCheck, Type_Basis) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("BASIS[sigma]"), kernel.parse("TYPE")));
}


TEST(DiracoqTypeCheck, Type_Ket) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("KTYPE[A]"), kernel.parse("TYPE")));

    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("KTYPE[T]"), kernel.parse("TYPE")));
}

TEST(DiracoqTypeCheck, Type_Bra) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("BTYPE[A]"), kernel.parse("TYPE")));

    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("BTYPE[T]"), kernel.parse("TYPE")));
}

TEST(DiracoqTypeCheck, Type_Opt) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("OTYPE[A, A]"), kernel.parse("TYPE")));

    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("OTYPE[T, A]"), kernel.parse("TYPE")));
}

TEST(DiracoqTypeCheck, Type_Scalar) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("STYPE"), kernel.parse("TYPE")));
}


TEST(DiracoqTypeCheck, Type_Set) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("SET[T]"), kernel.parse("TYPE")));
}

TEST(DiracoqTypeCheck, Term_Var1) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("x"), kernel.parse("TYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("x"), kernel.parse("TYPE")));
}

TEST(DiracoqTypeCheck, Term_Var2) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    kernel.def(kernel.register_symbol("f"), kernel.parse("FUN[x, T, x]"), kernel.parse("ARROW[T, T]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("f"), kernel.parse("ARROW[T, T]")));
}

TEST(DiracoqTypeCheck, Lam) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("FUN[x, T, x]"), kernel.parse("ARROW[T, T]")));
}

TEST(DiracoqTypeCheck, INDEX) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("IDX[sigma, 0K[sigma]]"), kernel.parse("FORALL[x, KTYPE[x]]")));
}

TEST(DiracoqTypeCheck, App_Arrow) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("ARROW[T, T]"));
    kernel.assum(kernel.register_symbol("x"), kernel.parse("T"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("APPLY[f, x]"), kernel.parse("T")));
}

TEST(DiracoqTypeCheck, App_Index) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("f"), kernel.parse("FORALL[x, KTYPE[x]]"));
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("INDEX"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("APPLY[f, sigma]"), kernel.parse("KTYPE[sigma]")));
}

TEST(DiracoqTypeCheck, Pair_Base) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("t1"), kernel.parse("BASIS[A]"));
    kernel.assum(kernel.register_symbol("t2"), kernel.parse("BASIS[B]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("PAIR[t1, t2]"), kernel.parse("BASIS[PROD[A, B]]")));
}

TEST(DiracoqTypeCheck, Sca_0) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("0"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, Sca_1) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("1"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, Sca_Delta) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a1"), kernel.parse("BASIS[A]"));
    kernel.assum(kernel.register_symbol("a2"), kernel.parse("BASIS[A]"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("DELTA[a1, a2]"), kernel.parse("STYPE")));

    kernel.assum(kernel.register_symbol("B"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("BASIS[B]"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("DELTA[a1, b]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, Sca_Add) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("c"), kernel.parse("STYPE"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("Plus[a, b, c]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, Sca_Mul) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("c"), kernel.parse("STYPE"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("Times[a, b, c]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, Sca_Conj) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("c"), kernel.parse("STYPE"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("Conjugate[a]"), kernel.parse("STYPE")));

    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("Conjugate[a, b]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, Sca_Dot) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KTYPE[T1]"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T1]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("DOT[B, K1]"), kernel.parse("STYPE")));

    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KTYPE[T2]"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("DOT[B, K2]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, Ket_0) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("0K[T]"), kernel.parse("KTYPE[T]")));
    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("0K[T, T]")));
}

TEST(DiracoqTypeCheck, Ket_Base) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("t"), kernel.parse("BASIS[T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("KET[t]"), kernel.parse("KTYPE[T]")));
}

TEST(DiracoqTypeCheck, Ket_Adj) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADJ[B]"), kernel.parse("KTYPE[T]")));
}

TEST(DiracoqTypeCheck, Ket_Scr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SCR[a, K]"), kernel.parse("KTYPE[T]")));
}

TEST(DiracoqTypeCheck, Ket_Add) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KTYPE[T]"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KTYPE[T]"));
    kernel.assum(kernel.register_symbol("K3"), kernel.parse("KTYPE[T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADD[K1, K2, K3]"), kernel.parse("KTYPE[T]")));
}

TEST(DiracoqTypeCheck, Ket_MulK) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));

    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KTYPE[T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("DOT[O1, K1]"), kernel.parse("KTYPE[T1]")));

    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OTYPE[T2, T1]"));  
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("DOT[O2, K1]"), kernel.parse("KTYPE[T1]")));
}

TEST(DiracoqTypeCheck, Ket_Tsr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));

    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KTYPE[T1]"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KTYPE[T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("TSR[K1, K2]"), kernel.parse("KTYPE[PROD[T1, T2]]")));
}

TEST(DiracoqTypeCheck, Bra_0) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("0B[T]"), kernel.parse("BTYPE[T]")));
    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("0B[T, T]")));
}

TEST(DiracoqTypeCheck, Bra_Base) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("t"), kernel.parse("BASIS[T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("BRA[t]"), kernel.parse("BTYPE[T]")));
}

TEST(DiracoqTypeCheck, Bra_Adj) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADJ[K]"), kernel.parse("BTYPE[T]")));
}

TEST(DiracoqTypeCheck, Bra_Scr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SCR[a, B]"), kernel.parse("BTYPE[T]")));
}

TEST(DiracoqTypeCheck, Bra_Add) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B1"), kernel.parse("BTYPE[T]"));
    kernel.assum(kernel.register_symbol("B2"), kernel.parse("BTYPE[T]"));
    kernel.assum(kernel.register_symbol("B3"), kernel.parse("BTYPE[T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADD[B1, B2, B3]"), kernel.parse("BTYPE[T]")));
}

TEST(DiracoqTypeCheck, Bra_MulB) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));

    kernel.assum(kernel.register_symbol("B1"), kernel.parse("BTYPE[T1]"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OTYPE[T1, T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("DOT[B1, O1]"), kernel.parse("BTYPE[T2]")));

    kernel.assum(kernel.register_symbol("B2"), kernel.parse("BTYPE[T2]"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("DOT[B2, O1]"), kernel.parse("BTYPE[T2]")));
}

TEST(DiracoqTypeCheck, Bra_Tsr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B1"), kernel.parse("BTYPE[T1]"));
    kernel.assum(kernel.register_symbol("B2"), kernel.parse("BTYPE[T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("TSR[B1, B2]"), kernel.parse("BTYPE[PROD[T1, T2]]")));
}

TEST(DiracoqTypeCheck, Opt_0) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("0O[T1, T2]"), kernel.parse("OTYPE[T1, T2]")));
    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("0O[T1]")));
}

TEST(DiracoqTypeCheck, Opt_1) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("1O[T1]"), kernel.parse("OTYPE[T1, T1]")));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("1O[T1, T2]"), kernel.parse("OTYPE[T1, T1]")));
}

TEST(DiracoqTypeCheck, Opt_Adj) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADJ[O]"), kernel.parse("OTYPE[T2, T1]")));
}

TEST(DiracoqTypeCheck, Opt_Scr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SCR[a, O]"), kernel.parse("OTYPE[T1, T2]")));
}

TEST(DiracoqTypeCheck, Opt_Add) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("O3"), kernel.parse("OTYPE[T1, T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADD[O1, O2, O3]"), kernel.parse("OTYPE[T1, T2]")));

    kernel.assum(kernel.register_symbol("O4"), kernel.parse("OTYPE[T2, T1]"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("ADD[O1, O4, O3]"), kernel.parse("OTYPE[T1, T2]")));
}
    
TEST(DiracoqTypeCheck, Opt_Outer) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T1]"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("DOT[K, B]"), kernel.parse("OTYPE[T1, T2]")));
}

TEST(DiracoqTypeCheck, Opt_MulO) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OTYPE[T2, T3]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("DOT[O1, O2]"), kernel.parse("OTYPE[T1, T3]")));

    kernel.assum(kernel.register_symbol("O3"), kernel.parse("OTYPE[T3, T1]"));
    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("DOT[O1, O3]")));
}

TEST(DiracoqTypeCheck, Opt_Tsr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T4"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OTYPE[T3, T4]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("TSR[O1, O2]"), kernel.parse("OTYPE[PROD[T1, T3], PROD[T2, T4]]")));
}

TEST(DiracoqTypeCheck, Set_U) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    
    EXPECT_TRUE(kernel.type_check(kernel.parse("USET[T]"), kernel.parse("SET[T]")));
}

TEST(DiracoqTypeCheck, Set_Prod) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));

    kernel.assum(kernel.register_symbol("S1"), kernel.parse("SET[T1]"));
    kernel.assum(kernel.register_symbol("S2"), kernel.parse("SET[T2]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("CATPROD[S1, S2]"), kernel.parse("SET[PROD[T1, T2]]")));
}

TEST(DiracoqTypeCheck, Sum_Scalar) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("s"), kernel.parse("SET[T]"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("ARROW[BASIS[T], STYPE]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SUM[s, f]"), kernel.parse("STYPE")));
}

TEST(DiracoqTypeCheck, Sum_Ket) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("s"), kernel.parse("SET[T]"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("ARROW[BASIS[T], KTYPE[sigma]]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SUM[s, f]"), kernel.parse("KTYPE[sigma]")));
}

TEST(DiracoqTypeCheck, Sum_Ket_2) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(
        kernel.parse("IDX[sigma, FUN[K, KTYPE[sigma], SUM[USET[sigma], FUN[x, BASIS[sigma], 0K[sigma]]]]]"), 
        kernel.parse("FORALL[sigma, ARROW[KTYPE[sigma], KTYPE[sigma]]]")));
}

TEST(DiracoqTypeCheck, Sum_Bra) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("s"), kernel.parse("SET[T]"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("ARROW[BASIS[T], BTYPE[sigma]]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SUM[s, f]"), kernel.parse("BTYPE[sigma]")));
}


TEST(DiracoqTypeCheck, Sum_Opt) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("tau"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("s"), kernel.parse("SET[T]"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("ARROW[BASIS[T], OTYPE[sigma, tau]]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SUM[s, f]"), kernel.parse("OTYPE[sigma, tau]")));
}


///////////////////////////////////////////////////////////////////////
TEST(DiracoqTypeCheck, Example1) {
    Kernel kernel;

    kernel.def(kernel.register_symbol("Tr"), kernel.parse("idx T => fun O : OTYPE[T, T] => Sum i in USET[T], <i| O |i>"));

    kernel.assum(kernel.register_symbol("c"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("OTYPE[T, T]"));
    kernel.assum(kernel.register_symbol("g"), kernel.parse("OTYPE[T, T]"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("c * (Tr T f) + Tr T g"), kernel.parse("STYPE")));
    
}
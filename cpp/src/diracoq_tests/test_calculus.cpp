#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqParsing, Basics1) {
    
    Kernel kernel;

    auto& bank = kernel.get_bank();
    auto& sig = kernel.get_sig();

    auto actual_res = kernel.parse("fun(x y apply(z x))");
    
    auto expected_res = bank.get_normal_term(
        sig.get_repr("fun"), {
            bank.get_normal_term(sig.get_repr("x"), {}),
            bank.get_normal_term(sig.get_repr("y"), {}),
            bank.get_normal_term(sig.get_repr("apply"), {
                bank.get_normal_term(sig.get_repr("z"), {}),
                bank.get_normal_term(sig.get_repr("x"), {})
            })
        }
    );

    EXPECT_EQ(actual_res, expected_res);
}


TEST(DiracoqParsing, Basics2) {
    
    Kernel kernel;

    EXPECT_EQ(kernel.term_to_string(kernel.parse("Type")), "Type");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("Type(Type)")), "Type(Type)");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("forall(x y apply(z x))")), "forall(x y apply(z x))");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("fun(x y apply(z x))")), "fun(x y apply(z x))");

}

TEST(DiracoqTypeCalc, successes) {
    Kernel kernel;

    // (Assum)
    kernel.assum(kernel.register_symbol("x"), kernel.parse("Type"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("x")), kernel.parse("Type"));
    kernel.env_pop();

    // (Def)
    kernel.def(kernel.register_symbol("f"), kernel.parse("fun (x Type x)"), kernel.parse("Arrow(Type Type)"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("f")), kernel.parse("Arrow(Type Type)"));

    // (App)
    kernel.assum(kernel.register_symbol("x"), kernel.parse("Type"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("apply(f x)")), kernel.parse("Type"));

}

TEST(DiracoqTypeCalc, errors) {
    Kernel kernel;

    EXPECT_ANY_THROW(kernel.env_pop());

    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("Type")));
}

TEST(DiracoqTypeCheck, Type_Base) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("Base"), kernel.parse("Type")));
}

TEST(DiracoqTypeCheck, Base_Subtype) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("T"), kernel.parse("Type")));
}

TEST(DiracoqTypeCheck, Type_Ket) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("Base"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("KType(A)"), kernel.parse("Type")));

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("KType(T)"), kernel.parse("Type")));
}

TEST(DiracoqTypeCheck, Type_Bra) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("Base"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("BType(A)"), kernel.parse("Type")));

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("BType(T)"), kernel.parse("Type")));
}

TEST(DiracoqTypeCheck, Type_Opt) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("Base"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("OType(A A)"), kernel.parse("Type")));

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("OType(T A)"), kernel.parse("Type")));
}

TEST(DiracoqTypeCheck, Type_Scalar) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("SType"), kernel.parse("Type")));
}


TEST(DiracoqTypeCheck, Type_Set) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("Set(T)"), kernel.parse("Type")));
}

TEST(DiracoqTypeCheck, Sca_0) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("0"), kernel.parse("SType")));
}

TEST(DiracoqTypeCheck, Sca_1) {
    Kernel kernel;

    EXPECT_TRUE(kernel.type_check(kernel.parse("1"), kernel.parse("SType")));
}

TEST(DiracoqTypeCheck, Sca_Delta) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("A"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("a1"), kernel.parse("A"));
    kernel.assum(kernel.register_symbol("a2"), kernel.parse("A"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("DELTA(a1 a2)"), kernel.parse("SType")));

    kernel.assum(kernel.register_symbol("B"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("B"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("DELTA(a1 b)"), kernel.parse("SType")));
}

TEST(DiracoqTypeCheck, Sca_Add) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("c"), kernel.parse("SType"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADDS(a b c)"), kernel.parse("SType")));

    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("ADDS(a Base)"), kernel.parse("SType")));
}

TEST(DiracoqTypeCheck, Sca_Mul) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("c"), kernel.parse("SType"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("MULS(a b c)"), kernel.parse("SType")));

    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("MULS(a Base)"), kernel.parse("SType")));
}

TEST(DiracoqTypeCheck, Sca_Conj) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("c"), kernel.parse("SType"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("CONJ(a)"), kernel.parse("SType")));

    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("CONJ(a b)"), kernel.parse("SType")));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("CONJ(Base)"), kernel.parse("SType")));
}

TEST(DiracoqTypeCheck, Sca_Dot) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KType(T1)"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BType(T1)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("DOT(B K1)"), kernel.parse("SType")));

    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KType(T2)"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("DOT(B K2)"), kernel.parse("SType")));
}

TEST(DiracoqTypeCheck, Base_Prod) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("Base"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("Prod(A B)"), kernel.parse("Type")));

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("Prod(T B)"), kernel.parse("Type")));
}

TEST(DiracoqTypeCheck, Pair_Base) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("A"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("t1"), kernel.parse("A"));
    kernel.assum(kernel.register_symbol("t2"), kernel.parse("B"));
    EXPECT_TRUE(kernel.type_check(kernel.parse("PAIR(t1 t2)"), kernel.parse("Prod(A B)")));
}

TEST(DiracoqTypeCheck, Ket_0) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("0K(T)"), kernel.parse("KType(T)")));
    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("0K(T T)")));
}

TEST(DiracoqTypeCheck, Ket_Base) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("t"), kernel.parse("T"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("KET(t)"), kernel.parse("KType(T)")));

    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("t2"), kernel.parse("T2"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("KET(t2)"), kernel.parse("KType(T2)")));
}

TEST(DiracoqTypeCheck, Ket_Adj) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BType(T)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADJ(B)"), kernel.parse("KType(T)")));
}

TEST(DiracoqTypeCheck, Ket_Scr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KType(T)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SCR(a K)"), kernel.parse("KType(T)")));
}

TEST(DiracoqTypeCheck, Ket_Add) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KType(T)"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KType(T)"));
    kernel.assum(kernel.register_symbol("K3"), kernel.parse("KType(T)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADD(K1 K2 K3)"), kernel.parse("KType(T)")));
}

TEST(DiracoqTypeCheck, Ket_MulK) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));

    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OType(T1 T2)"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KType(T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("MULK(O1 K1)"), kernel.parse("KType(T1)")));

    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OType(T2 T1)"));  
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("MULK(O2 K1)"), kernel.parse("KType(T1)")));
}

TEST(DiracoqTypeCheck, Ket_Tsr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));

    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KType(T1)"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KType(T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("TSR(K1 K2)"), kernel.parse("KType(Prod(T1 T2))")));
}

TEST(DiracoqTypeCheck, Bra_0) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("0B(T)"), kernel.parse("BType(T)")));
    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("0B(T T)")));
}

TEST(DiracoqTypeCheck, Bra_Base) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("t"), kernel.parse("T"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("BRA(t)"), kernel.parse("BType(T)")));

    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("t2"), kernel.parse("T2"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("BRA(t2)"), kernel.parse("BType(T2)")));
}

TEST(DiracoqTypeCheck, Bra_Adj) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KType(T)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADJ(K)"), kernel.parse("BType(T)")));
}

TEST(DiracoqTypeCheck, Bra_Scr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BType(T)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SCR(a B)"), kernel.parse("BType(T)")));
}

TEST(DiracoqTypeCheck, Bra_Add) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("B1"), kernel.parse("BType(T)"));
    kernel.assum(kernel.register_symbol("B2"), kernel.parse("BType(T)"));
    kernel.assum(kernel.register_symbol("B3"), kernel.parse("BType(T)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADD(B1 B2 B3)"), kernel.parse("BType(T)")));
}

TEST(DiracoqTypeCheck, Bra_MulB) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));

    kernel.assum(kernel.register_symbol("B1"), kernel.parse("BType(T1)"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OType(T1 T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("MULB(B1 O1)"), kernel.parse("BType(T2)")));

    kernel.assum(kernel.register_symbol("B2"), kernel.parse("BType(T2)"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("MULB(B2 O1)"), kernel.parse("BType(T2)")));
}

TEST(DiracoqTypeCheck, Bra_Tsr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("B1"), kernel.parse("BType(T1)"));
    kernel.assum(kernel.register_symbol("B2"), kernel.parse("BType(T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("TSR(B1 B2)"), kernel.parse("BType(Prod(T1 T2))")));
}

TEST(DiracoqTypeCheck, Opt_0) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("0O(T1 T2)"), kernel.parse("OType(T1 T2)")));
    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("0O(T1)")));
}

TEST(DiracoqTypeCheck, Opt_1) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("1O(T1)"), kernel.parse("OType(T1 T1)")));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("1O(T1 T2)"), kernel.parse("OType(T1 T1)")));
}

TEST(DiracoqTypeCheck, Opt_Adj) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OType(T1 T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADJ(O)"), kernel.parse("OType(T2 T1)")));
}

TEST(DiracoqTypeCheck, Opt_Scr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("a"), kernel.parse("SType"));
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OType(T1 T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SCR(a O)"), kernel.parse("OType(T1 T2)")));
}

TEST(DiracoqTypeCheck, Opt_Add) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OType(T1 T2)"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OType(T1 T2)"));
    kernel.assum(kernel.register_symbol("O3"), kernel.parse("OType(T1 T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("ADD(O1 O2 O3)"), kernel.parse("OType(T1 T2)")));

    kernel.assum(kernel.register_symbol("O4"), kernel.parse("OType(T2 T1)"));
    EXPECT_ANY_THROW(kernel.type_check(kernel.parse("ADD(O1 O4 O3)"), kernel.parse("OType(T1 T2)")));
}
    
TEST(DiracoqTypeCheck, Opt_Outer) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KType(T1)"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BType(T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("OUTER(K B)"), kernel.parse("OType(T1 T2)")));
}

TEST(DiracoqTypeCheck, Opt_MulO) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OType(T1 T2)"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OType(T2 T3)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("MULO(O1 O2)"), kernel.parse("OType(T1 T3)")));

    kernel.assum(kernel.register_symbol("O3"), kernel.parse("OType(T3 T1)"));
    EXPECT_ANY_THROW(kernel.calc_type(kernel.parse("MULO(O1 O3)")));
}

TEST(DiracoqTypeCheck, Opt_Tsr) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T4"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OType(T1 T2)"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OType(T3 T4)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("TSR(O1 O2)"), kernel.parse("OType(Prod(T1 T3) Prod(T2 T4))")));
}

TEST(DiracoqTypeCheck, Set_U) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    
    EXPECT_TRUE(kernel.type_check(kernel.parse("USET(T)"), kernel.parse("Set(T)")));
}

TEST(DiracoqTypeCheck, Set_Prod) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("Base"));

    kernel.assum(kernel.register_symbol("S1"), kernel.parse("Set(T1)"));
    kernel.assum(kernel.register_symbol("S2"), kernel.parse("Set(T2)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("CATPROD(S1 S2)"), kernel.parse("Set(Prod(T1 T2))")));
}

TEST(DiracoqTypeCheck, Sum_Scalar) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("s"), kernel.parse("Set(T)"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("Arrow(T SType)"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SUM(s f)"), kernel.parse("SType")));
}

TEST(DiracoqTypeCheck, Sum_Ket) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("s"), kernel.parse("Set(T)"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("Arrow(T KType(sigma))"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SUM(s f)"), kernel.parse("KType(sigma)")));
}

TEST(DiracoqTypeCheck, Sum_Bra) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("s"), kernel.parse("Set(T)"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("Arrow(T BType(sigma))"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SUM(s f)"), kernel.parse("BType(sigma)")));
}


TEST(DiracoqTypeCheck, Sum_Opt) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("tau"), kernel.parse("Base"));
    kernel.assum(kernel.register_symbol("s"), kernel.parse("Set(T)"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("Arrow(T OType(sigma tau))"));

    EXPECT_TRUE(kernel.type_check(kernel.parse("SUM(s f)"), kernel.parse("OType(sigma tau)")));
}
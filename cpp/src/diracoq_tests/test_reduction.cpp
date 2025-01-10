#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;



TEST(DiracoqReduction, variable_expand_K) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[A]"));

    auto term = kernel.parse("K");
    auto actual_res = variable_expand(kernel, term);
    auto expected_res = kernel.parse("SUM[USET[A], FUN[$0, BASIS[A], SCR[DOT[BRA[$0], K], KET[$0]]]]");

    EXPECT_EQ(*actual_res, *expected_res);
}


TEST(DiracoqReduction, variable_expand_K_apply) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("fK"), kernel.parse("BASIS[A] -> KTYPE[A]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("BASIS[A]"));

    auto term = kernel.parse("APPLY[fK, a]");
    auto actual_res = variable_expand(kernel, term);
    auto expected_res = kernel.parse("SUM[USET[A], FUN[$0, BASIS[A], SCR[DOT[BRA[$0], APPLY[fK, a]], KET[$0]]]]");

    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(DiracoqReduction, variable_expand_B) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[A]"));

    auto term = kernel.parse("B");
    auto actual_res = variable_expand(kernel, term);
    auto expected_res = kernel.parse("SUM[USET[A], FUN[$0, BASIS[A], SCR[DOT[B, KET[$0]], BRA[$0]]]]");

    EXPECT_EQ(*actual_res, *expected_res);
}


TEST(DiracoqReduction, variable_expand_O) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("A"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[A, B]"));

    auto term = kernel.parse("O");
    auto actual_res = variable_expand(kernel, term);
    auto expected_res = kernel.parse("SUM[USET[A], FUN[$0, BASIS[A], SUM[USET[B], FUN[$1, BASIS[B], SCR[DOT[BRA[$0], MULK[O, KET[$1]]], OUTER[KET[$0], BRA[$1]]]]]]]");

    EXPECT_EQ(*actual_res, *expected_res);
}


TEST(DiracoqReduction, wolfram_fullsimplify) {
    auto [ep, lp] = wstp::init_and_openlink(wstp::MACOS_ARGC, wstp::MACOS_ARGV);
    Kernel kernel(lp);

    auto actual_res = wolfram_fullsimplify(kernel, kernel.parse("Plus[1, 2]"));
    auto expected_res = kernel.parse("3");
    EXPECT_EQ(*actual_res, *expected_res);

    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));
    
    auto temp = wolfram_fullsimplify(kernel, kernel.parse("Plus[Minus[1], 1] . K"));
    actual_res = pos_rewrite_repeated(kernel, temp, {R_SCRK0});
    expected_res = kernel.parse("0K[T]");
    EXPECT_EQ(*actual_res, *expected_res);
}



/////////////////////////////////////////////////
// Test Rules


/**
 * @brief The helper function for testing a single rewriting rule.
 * 
 * @param kernel
 * @param rules
 * @param variables 
 * @param input 
 * @param expected 
 */
void TEST_RULE(Kernel& kernel, const vector<PosRewritingRule>& rules, string input, string expected) {
    auto term = kernel.parse(input);
    auto actual_res = pos_rewrite_repeated(kernel, term, rules);
    auto expected_res = kernel.parse(expected);
    cout << "actual_res: " << kernel.term_to_string(actual_res) << endl;
    cout << "expected_res: " << kernel.term_to_string(expected_res) << endl;
    EXPECT_EQ(*actual_res, *expected_res);
}

void TEST_RULE(const vector<PosRewritingRule>& rules, string input, string expected) {
    Kernel kernel;
    TEST_RULE(kernel, rules, input, expected);
}

TEST(DiracoqReduction, R_COMPO_SS) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    TEST_RULE(kernel, {R_COMPO_SS}, "COMPO[a, b]", "Times[a, b]");
} 

TEST(DiracoqReduction, R_COMPO_SK) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));
    TEST_RULE(kernel, {R_COMPO_SK}, "COMPO[a, K]", "SCR[a, K]");
}

TEST(DiracoqReduction, R_COMPO_SB) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));
    TEST_RULE(kernel, {R_COMPO_SB}, "COMPO[a, B]", "SCR[a, B]");
}

TEST(DiracoqReduction, R_COMPO_SO) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    TEST_RULE(kernel, {R_COMPO_SO}, "COMPO[a, O]", "SCR[a, O]");
}

TEST(DiracoqReduction, R_COMPO_KS) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));
    TEST_RULE(kernel, {R_COMPO_KS}, "COMPO[K, a]", "SCR[a, K]");
}

TEST(DiracoqReduction, R_COMPO_KK) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KTYPE[T1]"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KTYPE[T2]"));
    TEST_RULE(kernel, {R_COMPO_KK}, "COMPO[K1, K2]", "TSR[K1, K2]");
}

TEST(DiracoqReduction, R_COMPO_KB) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T2]"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T1]"));
    TEST_RULE(kernel, {R_COMPO_KB}, "COMPO[K, B]", "OUTER[K, B]");
}

TEST(DiracoqReduction, R_COMPO_BS) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    TEST_RULE(kernel, {R_COMPO_BS}, "COMPO[B, a]", "SCR[a, B]");
}
TEST(DiracoqReduction, R_COMPO_BK) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));
    TEST_RULE(kernel, {R_COMPO_BK}, "COMPO[B, K]", "DOT[B, K]");
}

TEST(DiracoqReduction, R_COMPO_BB) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B1"), kernel.parse("BTYPE[T1]"));
    kernel.assum(kernel.register_symbol("B2"), kernel.parse("BTYPE[T2]"));
    TEST_RULE(kernel, {R_COMPO_BB}, "COMPO[B1, B2]", "TSR[B1, B2]");
}

TEST(DiracoqReduction, R_COMPO_BO) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T1]"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    TEST_RULE(kernel, {R_COMPO_BO}, "COMPO[B, O]", "MULB[B, O]");
}

TEST(DiracoqReduction, R_COMPO_OS) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    TEST_RULE(kernel, {R_COMPO_OS}, "COMPO[O, a]", "SCR[a, O]");
}

TEST(DiracoqReduction, R_COMPO_OK) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T2]"));
    TEST_RULE(kernel, {R_COMPO_OK}, "COMPO[O, K]", "MULK[O, K]");
}

TEST(DiracoqReduction, R_COMPO_OO) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OTYPE[T2, T3]"));
    TEST_RULE(kernel, {R_COMPO_OO}, "COMPO[O1, O2]", "MULO[O1, O2]");
}

TEST(DiracoqReduction, R_COMPO_DD) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("r1"), kernel.parse("REG[T1]"));
    kernel.assum(kernel.register_symbol("r2"), kernel.parse("REG[T2]"));
    kernel.assum(kernel.register_symbol("D1"), kernel.parse("DTYPE[RSET[r1], RSET[r2]]"));
    kernel.assum(kernel.register_symbol("D2"), kernel.parse("DTYPE[RSET[r2], RSET[r1]]"));
    TEST_RULE(kernel, {R_COMPO_DD}, "COMPO[D1, D2]", "LDOT[D1, D2]");
}

TEST(DiracoqReduction, R_COMPO_ARROW) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("ARROW[T1, T2]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("T1"));
    TEST_RULE(kernel, {R_COMPO_ARROW}, "COMPO[f, a]", "APPLY[f, a]");
}

TEST(DiracoqReduction, R_COMPO_FORALL) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("FORALL[sigma, T]"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("INDEX"));
    TEST_RULE(kernel, {R_COMPO_FORALL}, "COMPO[f, a]", "APPLY[f, a]");
}

TEST(DiracoqReduction, R_STAR_PROD) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    TEST_RULE(kernel, {R_STAR_PROD}, "STAR[T, T]", "PROD[T, T]");
}

TEST(DiracoqReduction, R_STAR_MULS) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    TEST_RULE(kernel, {R_STAR_MULS}, "STAR[a, b, a]", "Times[a, b, a]");
}

TEST(DiracoqReduction, R_STAR_TSRO) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O1"), kernel.parse("OTYPE[T1, T2]"));
    kernel.assum(kernel.register_symbol("O2"), kernel.parse("OTYPE[T2, T3]"));
    TEST_RULE(kernel, {R_STAR_TSRO}, "STAR[O1, O2]", "TSR[O1, O2]");
}

TEST(DiracoqReduction, R_STAR_CATPROD) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("S1"), kernel.parse("SET[T1]"));
    kernel.assum(kernel.register_symbol("S2"), kernel.parse("SET[T2]"));
    TEST_RULE(kernel, {R_STAR_CATPROD}, "STAR[S1, S2]", "CATPROD[S1, S2]");
}

TEST(DiracoqReduction, R_STAR_LTSR) {
    Kernel kernel;
    
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("r1"), kernel.parse("REG[T1]"));
    kernel.assum(kernel.register_symbol("r2"), kernel.parse("REG[T2]"));
    kernel.assum(kernel.register_symbol("D1"), kernel.parse("DTYPE[RSET[r1], RSET[r2]]"));
    kernel.assum(kernel.register_symbol("D2"), kernel.parse("DTYPE[RSET[r2], RSET[r1]]"));
    TEST_RULE(kernel, {R_STAR_LTSR}, "STAR[D1, D2]", "LTSR[D1, D2]");
}

TEST(DiracoqReduction, R_ADDG_ADDS) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("a"), kernel.parse("STYPE"));
    kernel.assum(kernel.register_symbol("b"), kernel.parse("STYPE"));
    TEST_RULE(kernel, {R_ADDG_ADDS}, "ADDG[a, b]", "Plus[a, b]");
}

TEST(DiracoqReduction, R_ADDG_ADD) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K1"), kernel.parse("KTYPE[T1]"));
    kernel.assum(kernel.register_symbol("K2"), kernel.parse("KTYPE[T1]"));
    TEST_RULE(kernel, {R_ADDG_ADD}, "ADDG[K1, K2]", "ADD[K1, K2]");
}

TEST(DiracoqReduction, R_SSUM) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    TEST_RULE(kernel, {R_SSUM}, "SSUM[i, USET[T1], KET[i]]", "SUM[USET[T1], FUN[i, BASIS[T1], KET[i]]]");
}

/////////////////////////////////////////////
// main rules unit test

TEST(DiracoqReduction, R_BETA_ARROW) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("T"));
    auto term = kernel.parse("APPLY[FUN[x, T, x], a]");
    auto actual_res = pos_rewrite_repeated(kernel, term, {R_BETA_ARROW});
    auto expected_res = kernel.parse("a");
    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(DiracoqReduction, R_BETA_INDEX) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("sigma"), kernel.parse("INDEX"));
    auto term = kernel.parse("APPLY[IDX[sigma, 0K[sigma]], sigma]");
    auto actual_res = pos_rewrite_repeated(kernel, term, {R_BETA_INDEX});
    auto expected_res = kernel.parse("0K[sigma]");
    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(DiracoqReduction, R_DELTA) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("TYPE"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("T"));
    kernel.def(kernel.register_symbol("f"), kernel.parse("FUN[x, T, x]"));
    auto term = kernel.parse("APPLY[f, a]");
    auto actual_res = pos_rewrite_repeated(kernel, term, {R_DELTA, R_BETA_ARROW});
    auto expected_res = kernel.parse("a");
    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(DiracoqReduction, R_FLATTEN) {
    TEST_RULE({R_FLATTEN}, "Times[a, Times[a, b], b]", "Times[a, a, b, b]");
    TEST_RULE({R_FLATTEN}, "Times[a, Plus[a, b], b]", "Times[a, Plus[a, b], b]");
    TEST_RULE({R_FLATTEN}, "ADD[a, ADD[b, c], d]", "ADD[a, b, c, d]");
}

TEST(DiracoqReduction, R_ADDS0) {
    TEST_RULE({R_ADDSID, R_ADDS0}, "Plus[0, a, b]", "Plus[a, b]");
    TEST_RULE({R_ADDSID, R_ADDS0}, "Plus[0, a, b, 0, 0]", "Plus[a, b]");
    TEST_RULE({R_ADDSID, R_ADDS0}, "Plus[0, b]", "b");
    TEST_RULE({R_ADDS0}, "Plus[0, 0]", "Plus[0]");
    TEST_RULE({R_ADDS0}, "Plus[0]", "Plus[0]");
}

TEST(DiracoqReduction, R_MULS0) {
    TEST_RULE({R_MULSID, R_MULS0}, "Times[0, a, b]", "0");
}

TEST(DiracoqReduction, R_MULS1) {
    TEST_RULE({R_MULSID, R_MULS1}, "Times[1, a, b]", "Times[a, b]");
    TEST_RULE({R_MULSID, R_MULS1}, "Times[1, a, 1, b, 1]", "Times[a, b]");
    TEST_RULE({R_MULSID, R_MULS1}, "Times[1, b]", "b");
    TEST_RULE({R_MULS1}, "Times[1, 1]", "Times[1]");
    TEST_RULE({R_MULS1}, "Times[1]", "Times[1]");
}

TEST(DiracoqReduction, R_MULS2) {
    TEST_RULE({R_MULS2}, "Times[a, Plus[b, c]]", "Plus[Times[a, b], Times[a, c]]");
    TEST_RULE({R_MULS2}, "Times[a, Plus[b, c], b]", "Plus[Times[a, b, b], Times[a, c, b]]");
}

TEST(DiracoqReduction, R_MULS2_nasty) {
    TEST_RULE({R_MULS2}, "Times[Plus[b, c]]", "Times[Plus[b, c]]");
}

TEST(DiracoqReduction, R_CONJ0) {
    TEST_RULE({R_CONJ0}, "Conjugate[0]", "0");
    TEST_RULE({R_CONJ0}, "Plus[Conjugate[0], a]", "Plus[0, a]");
}

TEST(DiracoqReduction, R_CONJ1) {
    TEST_RULE({R_CONJ1}, "Conjugate[1]", "1");
    TEST_RULE({R_CONJ1}, "Plus[Conjugate[1], a]", "Plus[1, a]");
}

TEST(DiracoqReduction, R_CONJ2) {
    TEST_RULE({R_CONJ2}, "Conjugate[Plus[a, b]]", "Plus[Conjugate[a], Conjugate[b]]");
    TEST_RULE({R_CONJ2}, "Conjugate[Plus[a, b, c]]", "Plus[Conjugate[a], Conjugate[b], Conjugate[c]]");
}

TEST(DiracoqReduction, R_CONJ3) {
    TEST_RULE({R_CONJ3}, "Conjugate[Times[a, b]]", "Times[Conjugate[a], Conjugate[b]]");
    TEST_RULE({R_CONJ3}, "Conjugate[Times[a, b, c]]", "Times[Conjugate[a], Conjugate[b], Conjugate[c]]");
}

TEST(DiracoqReduction, R_CONJ4) {
    TEST_RULE({R_CONJ4}, "Conjugate[Conjugate[a]]", "a");
    TEST_RULE({R_CONJ4}, "Conjugate[Conjugate[Plus[a, b]]]", "Plus[a, b]");
}

TEST(DiracoqReduction, R_CONJ5) {
    TEST_RULE({R_CONJ5}, "Conjugate[DELTA[a, b]]", "DELTA[a, b]");
}

TEST(DiracoqReduction, R_CONJ6) {
    TEST_RULE({R_CONJ6}, "Conjugate[DOT[B, K]]", "DOT[ADJ[K], ADJ[B]]");
}

TEST(DiracoqReduction, R_DOT0) {
    TEST_RULE({R_DOT0}, "DOT[0B[sigma], K]", "0");
}

TEST(DiracoqReduction, R_DOT1) {
    TEST_RULE({R_DOT1}, "DOT[B, 0K[sigma]]", "0");
}

TEST(DiracoqReduction, R_DOT2) {
    TEST_RULE({R_DOT2}, "DOT[SCR[a, B], K]", "Times[a, DOT[B, K]]");
}

TEST(DiracoqReduction, R_DOT3) {
    TEST_RULE({R_DOT3}, "DOT[B, SCR[a, K]]", "Times[a, DOT[B, K]]");
}

TEST(DiracoqReduction, R_DOT4) {
    TEST_RULE({R_DOT4}, "DOT[ADD[B1], K]", "Plus[DOT[B1, K]]");
    TEST_RULE({R_DOT4}, "DOT[ADD[B1, B2], K]", "Plus[DOT[B1, K], DOT[B2, K]]");
    TEST_RULE({R_DOT4}, "DOT[ADD[B1, B2, B3], K]", "Plus[DOT[B1, K], DOT[B2, K], DOT[B3, K]]");
}

TEST(DiracoqReduction, R_DOT5) {
    TEST_RULE({R_DOT5}, "DOT[B, ADD[K1]]", "Plus[DOT[B, K1]]");
    TEST_RULE({R_DOT5}, "DOT[B, ADD[K1, K2]]", "Plus[DOT[B, K1], DOT[B, K2]]");
    TEST_RULE({R_DOT5}, "DOT[B, ADD[K1, K2, K3]]", "Plus[DOT[B, K1], DOT[B, K2], DOT[B, K3]]");
}

TEST(DiracoqReduction, R_DOT6) {
    TEST_RULE({R_DOT6}, "DOT[BRA[s], KET[t]]", "DELTA[s, t]");
}

TEST(DiracoqReduction, R_DOT7) {
    TEST_RULE({R_DOT7}, "DOT[TSR[B1, B2], KET[PAIR[s, t]]]", "Times[DOT[B1, KET[s]], DOT[B2, KET[t]]]");
}

TEST(DiracoqReduction, R_DOT8) {
    TEST_RULE({R_DOT8}, "DOT[BRA[PAIR[s, t]], TSR[K1, K2]]", "Times[DOT[BRA[s], K1], DOT[BRA[t], K2]]");
}

TEST(DiracoqReduction, R_DOT9) {
    TEST_RULE({R_DOT9}, "DOT[TSR[B1, B2], TSR[K1, K2]]", "Times[DOT[B1, K1], DOT[B2, K2]]");
}

TEST(DiracoqReduction, R_DOT10) {
    TEST_RULE({R_DOT10}, "DOT[MULB[B, O], K]", "DOT[B, MULK[O, K]]");
}

TEST(DiracoqReduction, R_DOT11) {
    TEST_RULE({R_DOT11}, "DOT[BRA[PAIR[s, t]], MULK[TSR[O1, O2], K]]", "DOT[TSR[MULB[BRA[s], O1], MULB[BRA[t], O2]], K]");
}

TEST(DiracoqReduction, R_DOT12) {
    TEST_RULE({R_DOT12}, "DOT[TSR[B1, B2], MULK[TSR[O1, O2], K]]", "DOT[TSR[MULB[B1, O1], MULB[B2, O2]], K]");
}

TEST(DiracoqReduction, R_DELTA0) {
    TEST_RULE({R_DELTA0}, "DELTA[a, a]", "1");
}

TEST(DiracoqReduction, R_DELTA1) {
    TEST_RULE({R_DELTA1}, "DELTA[PAIR[a, b], PAIR[c, d]]", "Times[DELTA[a, c], DELTA[b, d]]");
}

TEST(DiracoqReduction, R_SCR0) {
    TEST_RULE({R_SCR0}, "SCR[1, X]", "X");
}

TEST(DiracoqReduction, R_SCR1) {
    TEST_RULE({R_SCR1}, "SCR[a, SCR[b, X]]", "SCR[Times[a, b], X]");
}

TEST(DiracoqReduction, R_SCR2) {
    TEST_RULE({R_SCR2}, "SCR[a, ADD[X1]]", "ADD[SCR[a, X1]]");
    TEST_RULE({R_SCR2}, "SCR[a, ADD[X1, X2]]", "ADD[SCR[a, X1], SCR[a, X2]]");
}

TEST(DiracoqReduction, R_SCRK0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T]"));

    TEST_RULE(kernel, {R_SCRK0}, "SCR[0, K]", "0K[T]");
}

TEST(DiracoqReduction, R_SCRK1) {
    TEST_RULE({R_SCRK1}, "SCR[a, 0K[T]]", "0K[T]");
}

TEST(DiracoqReduction, R_SCRB0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T]"));

    TEST_RULE(kernel, {R_SCRB0}, "SCR[0, B]", "0B[T]");
}

TEST(DiracoqReduction, R_SCRB1) {
    TEST_RULE({R_SCRB1}, "SCR[a, 0B[T]]", "0B[T]");
}

TEST(DiracoqReduction, R_SCRO0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));

    TEST_RULE(kernel, {R_SCRO0}, "SCR[0, O]", "0O[T1, T2]");
}

TEST(DiracoqReduction, R_SCRO1) {
    TEST_RULE({R_SCRO1}, "SCR[a, 0O[T1, T2]]", "0O[T1, T2]");
}

TEST(DiracoqReduction, R_ADDID) {
    TEST_RULE({R_ADDID}, "ADD[X]", "X");
}

TEST(DiracoqReduction, R_ADD0) {
    TEST_RULE({R_ADD0}, "ADD[X, X, Y, Z]", "ADD[Y, Z, SCR[Plus[1, 1], X]]");
    TEST_RULE({R_ADD0}, "ADD[a, X, b, X, Y, Z]", "ADD[a, b, Y, Z, SCR[Plus[1, 1], X]]");
}

TEST(DiracoqReduction, R_ADD1) {
    TEST_RULE({R_ADD1}, "ADD[X, Y, Z, SCR[a, X], W]", "ADD[Y, Z, W, SCR[Plus[1, a], X]]");
}

TEST(DiracoqReduction, R_ADD2) {
    TEST_RULE({R_ADD2}, "ADD[SCR[a, X], Y, X, Z]", "ADD[Y, Z, SCR[Plus[a, 1], X]]");
}

TEST(DiracoqReduction, R_ADD3) {
    TEST_RULE({R_ADD3}, "ADD[SCR[b, X], Y, Z, SCR[a, X], W]", "ADD[Y, Z, W, SCR[Plus[b, a], X]]");
}

TEST(DiracoqReduction, R_ADDK0) {
    TEST_RULE({R_ADDK0}, "ADD[K1, K2, 0K[T], K3]", "ADD[K1, K2, K3]");
}

TEST(DiracoqReduction, R_ADDB0) {
    TEST_RULE({R_ADDB0}, "ADD[B1, B2, 0B[T], B3]", "ADD[B1, B2, B3]");
}

TEST(DiracoqReduction, R_ADDO0) {
    TEST_RULE({R_ADDO0}, "ADD[O1, O2, 0O[T1, T2], O3]", "ADD[O1, O2, O3]");
}

TEST(DiracoqReduction, R_ADJ0) {
    TEST_RULE({R_ADJ0}, "ADJ[ADJ[X]]", "X");
}

TEST(DiracoqReduction, R_ADJ1) {
    TEST_RULE({R_ADJ1}, "ADJ[SCR[a, X]]", "SCR[Conjugate[a], ADJ[X]]");
}

TEST(DiracoqReduction, R_ADJ2) {
    TEST_RULE({R_ADJ2}, "ADJ[ADD[X1, X2, X3]]", "ADD[ADJ[X1], ADJ[X2], ADJ[X3]]");
}

TEST(DiracoqReduction, R_ADJ3) {
    TEST_RULE({R_ADJ3}, "ADJ[TSR[X, Y]]", "TSR[ADJ[X], ADJ[Y]]");
}

TEST(DiracoqReduction, R_ADJK0) {
    TEST_RULE({R_ADJK0}, "ADJ[0B[T]]", "0K[T]");
}

TEST(DiracoqReduction, R_ADJK1) {
    TEST_RULE({R_ADJK1}, "ADJ[BRA[t]]", "KET[t]");
}

TEST(DiracoqReduction, R_ADJK2) {
    TEST_RULE({R_ADJK2}, "ADJ[MULB[B, O]]", "MULK[ADJ[O], ADJ[B]]");
}

TEST(DiracoqReduction, R_ADJB0) {
    TEST_RULE({R_ADJB0}, "ADJ[0K[T]]", "0B[T]");
}

TEST(DiracoqReduction, R_ADJB1) {
    TEST_RULE({R_ADJB1}, "ADJ[KET[t]]", "BRA[t]");
}

TEST(DiracoqReduction, R_ADJB2) {
    TEST_RULE({R_ADJB2}, "ADJ[MULK[O, K]]", "MULB[ADJ[K], ADJ[O]]");
}

TEST(DiracoqReduction, R_ADJO0) {
    TEST_RULE({R_ADJO0}, "ADJ[0O[T1, T2]]", "0O[T2, T1]");
}

TEST(DiracoqReduction, R_ADJO1) {
    TEST_RULE({R_ADJO1}, "ADJ[1O[T]]", "1O[T]");
}

TEST(DiracoqReduction, R_ADJO2) {
    TEST_RULE({R_ADJO2}, "ADJ[OUTER[K, B]]", "OUTER[ADJ[B], ADJ[K]]");
}

TEST(DiracoqReduction, R_ADJO3) {
    TEST_RULE({R_ADJO3}, "ADJ[MULO[O1, O2]]", "MULO[ADJ[O2], ADJ[O1]]");
}

TEST(DiracoqReduction, R_TSR0) {
    TEST_RULE({R_TSR0}, "TSR[SCR[a, X1], X2]", "SCR[a, TSR[X1, X2]]");
}

TEST(DiracoqReduction, R_TSR1) {
    TEST_RULE({R_TSR1}, "TSR[X1, SCR[a, X2]]", "SCR[a, TSR[X1, X2]]");
}

TEST(DiracoqReduction, R_TSR2) {
    TEST_RULE({R_TSR2}, "TSR[ADD[X1, X2, X3], Y]", "ADD[TSR[X1, Y], TSR[X2, Y], TSR[X3, Y]]");
}

TEST(DiracoqReduction, R_TSR3) {
    TEST_RULE({R_TSR3}, "TSR[Y, ADD[X1, X2, X3]]", "ADD[TSR[Y, X1], TSR[Y, X2], TSR[Y, X3]]");
}

TEST(DiracoqReduction, R_TSRK0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T2]"));
    TEST_RULE(kernel, {R_TSRK0}, "TSR[0K[T1], K]", "0K[PROD[T1, T2]]");
}

TEST(DiracoqReduction, R_TSRK1) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T1]"));
    TEST_RULE(kernel, {R_TSRK1}, "TSR[K, 0K[T2]]", "0K[PROD[T1, T2]]");
}

TEST(DiracoqReduction, R_TSRK2) {
    TEST_RULE({R_TSRK2}, "TSR[KET[s], KET[t]]", "KET[PAIR[s, t]]");
}

TEST(DiracoqReduction, R_TSRB0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T2]"));
    TEST_RULE(kernel, {R_TSRB0}, "TSR[0B[T1], B]", "0B[PROD[T1, T2]]");
}

TEST(DiracoqReduction, R_TSRB1) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T1]"));
    TEST_RULE(kernel, {R_TSRB1}, "TSR[B, 0B[T2]]", "0B[PROD[T1, T2]]");
}
TEST(DiracoqReduction, R_TSRB2) {
    TEST_RULE({R_TSRB2}, "TSR[BRA[s], BRA[t]]", "BRA[PAIR[s, t]]");
}

TEST(DiracoqReduction, R_TSRO0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T4"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T3, T4]"));
    TEST_RULE(kernel, {R_TSRO0}, "TSR[0O[T1, T2], O]", "0O[PROD[T1, T3], PROD[T2, T4]]");
}

TEST(DiracoqReduction, R_TSRO1) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T4"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    TEST_RULE(kernel, {R_TSRO1}, "TSR[O, 0O[T3, T4]]", "0O[PROD[T1, T3], PROD[T2, T4]]");
}

TEST(DiracoqReduction, R_TSRO2) {
    TEST_RULE({R_TSRO2}, "TSR[1O[T1], 1O[T2]]", "1O[PROD[T1, T2]]");
}

TEST(DiracoqReduction, R_TSRO3) {
    TEST_RULE({R_TSRO3}, "TSR[OUTER[K1, B1], OUTER[K2, B2]]", "OUTER[TSR[K1, K2], TSR[B1, B2]]");
}

TEST(DiracoqReduction, R_MULK0) {
    TEST_RULE({R_MULK0}, "MULK[0O[T1, T2], K]", "0K[T1]");
}

TEST(DiracoqReduction, R_MULK1) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    TEST_RULE(kernel, {R_MULK1}, "MULK[O, 0K[T2]]", "0K[T1]");
}

TEST(DiracoqReduction, R_MULK2) {
    TEST_RULE({R_MULK2}, "MULK[1O[T], K]", "K");
}

TEST(DiracoqReduction, R_MULK3) {
    TEST_RULE({R_MULK3}, "MULK[SCR[a, O], K]", "SCR[a, MULK[O, K]]");
}

TEST(DiracoqReduction, R_MULK4) {
    TEST_RULE({R_MULK4}, "MULK[O, SCR[a, K]]", "SCR[a, MULK[O, K]]");
}

TEST(DiracoqReduction, R_MULK5) {
    TEST_RULE({R_MULK5}, "MULK[ADD[O1, O2], K]", "ADD[MULK[O1, K], MULK[O2, K]]");
}

TEST(DiracoqReduction, R_MULK6) {
    TEST_RULE({R_MULK6}, "MULK[O, ADD[K1, K2]]", "ADD[MULK[O, K1], MULK[O, K2]]");
}

TEST(DiracoqReduction, R_MULK7) {
    TEST_RULE({R_MULK7}, "MULK[OUTER[K1, B], K2]", "SCR[DOT[B, K2], K1]");
}

TEST(DiracoqReduction, R_MULK8) {
    TEST_RULE({R_MULK8}, "MULK[MULO[O1, O2], K]", "MULK[O1, MULK[O2, K]]");
}

TEST(DiracoqReduction, R_MULK9) {
    TEST_RULE({R_MULK9}, "MULK[TSR[O1, O2], MULK[TSR[O3, O4], K]]", "MULK[TSR[MULO[O1, O3], MULO[O2, O4]], K]");
}

TEST(DiracoqReduction, R_MULK10) {
    TEST_RULE({R_MULK10}, "MULK[TSR[O1, O2], KET[PAIR[s, t]]]", "TSR[MULK[O1, KET[s]], MULK[O2, KET[t]]]");
}

TEST(DiracoqReduction, R_MULK11) {
    TEST_RULE({R_MULK11}, "MULK[TSR[O1, O2], TSR[K1, K2]]", "TSR[MULK[O1, K1], MULK[O2, K2]]");
}

TEST(DiracoqReduction, R_MULB0) {
    TEST_RULE({R_MULB0}, "MULB[B, 0O[T1, T2]]", "0B[T2]");
}

TEST(DiracoqReduction, R_MULB1) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    TEST_RULE(kernel, {R_MULB1}, "MULB[0B[T1], O]", "0B[T2]");
}

TEST(DiracoqReduction, R_MULB2) {
    TEST_RULE({R_MULB2}, "MULB[B, 1O[T]]", "B");
}

TEST(DiracoqReduction, R_MULB3) {
    TEST_RULE({R_MULB3}, "MULB[SCR[a, B], O]", "SCR[a, MULB[B, O]]");
}

TEST(DiracoqReduction, R_MULB4) {
    TEST_RULE({R_MULB4}, "MULB[B, SCR[a, O]]", "SCR[a, MULB[B, O]]");
}

TEST(DiracoqReduction, R_MULB5) {
    TEST_RULE({R_MULB5}, "MULB[ADD[B1, B2], O]", "ADD[MULB[B1, O], MULB[B2, O]]");
}

TEST(DiracoqReduction, R_MULB6) {
    TEST_RULE({R_MULB6}, "MULB[B, ADD[O1, O2]]", "ADD[MULB[B, O1], MULB[B, O2]]");
}

TEST(DiracoqReduction, R_MULB7) {
    TEST_RULE({R_MULB7}, "MULB[B1, OUTER[K, B2]]", "SCR[DOT[B1, K], B2]");
}

TEST(DiracoqReduction, R_MULB8) {
    TEST_RULE({R_MULB8}, "MULB[B, MULO[O1, O2]]", "MULB[MULB[B, O1], O2]");
}

TEST(DiracoqReduction, R_MULB9) {
    TEST_RULE({R_MULB9}, "MULB[MULB[B, TSR[O1, O2]], TSR[O3, O4]]", "MULB[B, TSR[MULO[O1, O3], MULO[O2, O4]]]");
}

TEST(DiracoqReduction, R_MULB10) {
    TEST_RULE({R_MULB10}, "MULB[BRA[PAIR[s, t]], TSR[O1, O2]]", "TSR[MULB[BRA[s], O1], MULB[BRA[t], O2]]");
}

TEST(DiracoqReduction, R_MULB11) {
    TEST_RULE({R_MULB11}, "MULB[TSR[B1, B2], TSR[O1, O2]]", "TSR[MULB[B1, O1], MULB[B2, O2]]");
}

TEST(DiracoqReduction, R_OUTER0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T2]"));
    TEST_RULE(kernel, {R_OUTER0}, "OUTER[0K[T1], B]", "0O[T1, T2]");
}

TEST(DiracoqReduction, R_OUTER1) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T1]"));
    TEST_RULE(kernel, {R_OUTER1}, "OUTER[K, 0B[T2]]", "0O[T1, T2]");
}

TEST(DiracoqReduction, R_OUTER2) {
    TEST_RULE({R_OUTER2}, "OUTER[SCR[a, K], B]", "SCR[a, OUTER[K, B]]");
}

TEST(DiracoqReduction, R_OUTER3) {
    TEST_RULE({R_OUTER3}, "OUTER[K, SCR[a, B]]", "SCR[a, OUTER[K, B]]");
}

TEST(DiracoqReduction, R_OUTER4) {
    TEST_RULE({R_OUTER4}, "OUTER[ADD[K1, K2], B]", "ADD[OUTER[K1, B], OUTER[K2, B]]");
}

TEST(DiracoqReduction, R_OUTER5) {
    TEST_RULE({R_OUTER5}, "OUTER[K, ADD[B1, B2]]", "ADD[OUTER[K, B1], OUTER[K, B2]]");
}
TEST(DiracoqReduction, R_MULO0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T2, T3]"));
    TEST_RULE(kernel, {R_MULO0}, "MULO[0O[T1, T2], O]", "0O[T1, T3]");
}

TEST(DiracoqReduction, R_MULO1) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T3"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T2]"));
    TEST_RULE(kernel, {R_MULO1}, "MULO[O, 0O[T2, T3]]", "0O[T1, T3]");
}

TEST(DiracoqReduction, R_MULO2) {
    TEST_RULE({R_MULO2}, "MULO[1O[T], O]", "O");
}

TEST(DiracoqReduction, R_MULO3) {
    TEST_RULE({R_MULO3}, "MULO[O, 1O[T]]", "O");
}

TEST(DiracoqReduction, R_MULO4) {
    TEST_RULE({R_MULO4}, "MULO[OUTER[K, B], O]", "OUTER[K, MULB[B, O]]");
}

TEST(DiracoqReduction, R_MULO5) {
    TEST_RULE({R_MULO5}, "MULO[O, OUTER[K, B]]", "OUTER[MULK[O, K], B]");
}

TEST(DiracoqReduction, R_MULO6) {
    TEST_RULE({R_MULO6}, "MULO[SCR[a, O], O]", "SCR[a, MULO[O, O]]");
}

TEST(DiracoqReduction, R_MULO7) {
    TEST_RULE({R_MULO7}, "MULO[O, SCR[a, O]]", "SCR[a, MULO[O, O]]");
}

TEST(DiracoqReduction, R_MULO8) {
    TEST_RULE({R_MULO8}, "MULO[ADD[O1, O2], O]", "ADD[MULO[O1, O], MULO[O2, O]]");
}

TEST(DiracoqReduction, R_MULO9) {
    TEST_RULE({R_MULO9}, "MULO[O, ADD[O1, O2]]", "ADD[MULO[O, O1], MULO[O, O2]]");
}

TEST(DiracoqReduction, R_MULO10) {
    TEST_RULE({R_MULO10}, "MULO[MULO[O1, O2], O]", "MULO[O1, MULO[O2, O]]");
}

TEST(DiracoqReduction, R_MULO11) {
    TEST_RULE({R_MULO11}, "MULO[TSR[O1, O2], TSR[O3, O4]]", "TSR[MULO[O1, O3], MULO[O2, O4]]");
}

TEST(DiracoqReduction, R_MULO12) {
    TEST_RULE({R_MULO12}, "MULO[TSR[O1, O2], MULO[TSR[O3, O4], O]]", "MULO[TSR[MULO[O1, O3], MULO[O2, O4]], O]");
}

TEST(DiracoqReduction, R_SET0) {
    TEST_RULE({R_SET0}, "CATPROD[USET[T1], USET[T2]]", "USET[PROD[T1, T2]]");
}

TEST(DiracoqReduction, R_SUM_CONST0) {
    TEST_RULE({R_SUM_CONST0}, "SUM[s, FUN[x, T, 0]]", "0");
}

TEST(DiracoqReduction, R_SUM_CONST1) {
    TEST_RULE({R_SUM_CONST1}, "SUM[s, FUN[x, T, 0K[sigma]]]", "0K[sigma]");
}

TEST(DiracoqReduction, R_SUM_CONST2) {
    TEST_RULE({R_SUM_CONST2}, "SUM[s, FUN[x, T, 0B[sigma]]]", "0B[sigma]");
}

TEST(DiracoqReduction, R_SUM_CONST3) {
    TEST_RULE({R_SUM_CONST3}, "SUM[s, FUN[x, T, 0O[sigma, tau]]]", "0O[sigma, tau]");
}

TEST(DiracoqReduction, R_SUM_CONST4) {
    TEST_RULE({R_SUM_CONST4}, "1O[T]", "SUM[USET[T], FUN[$0, BASIS[T], OUTER[KET[$0], BRA[$0]]]]");
}

TEST(DiracoqReduction, R_SUM_ELIM0) {
    TEST_RULE({R_SUM_ELIM0}, "SUM[USET[T], FUN[i, T, DELTA[i, j]]]", "1");
    TEST_RULE({R_SUM_ELIM0}, 
        R"(
        SUM[USET[T], FUN[i, T,
            SUM[USET[T2], FUN[k, T2, 
                DELTA[i, j]
            ]]
        ]]
        )", 
        "SUM[USET[T2], FUN[k, T2, 1]]");
    TEST_RULE({R_SUM_ELIM0}, 
        R"(
        SUM[USET[T], FUN[i, T,
            SUM[USET[T2], FUN[k, T2, 
                DELTA[i, i]
            ]]
        ]]
        )", 
        R"(
        SUM[USET[T], FUN[i, T,
            SUM[USET[T2], FUN[k, T2, 
                DELTA[i, i]
            ]]
        ]]
        )");
}

TEST(DiracoqReduction, R_SUM_ELIM1) {
    TEST_RULE({R_SUM_ELIM1}, 
        R"(
            SUM[USET[T], FUN[i, T, 
                Times[
                    DOT[BRA[i], KET[i]],
                    DELTA[i, j]
                ]
            ]]
        )", 
        "Times[DOT[BRA[j], KET[j]]]");
    TEST_RULE({R_SUM_ELIM1},
        R"(
        SUM[USET[T], FUN[i, T,
            SUM[USET[T2], FUN[k, T2, 
                Times[
                    DELTA[i, j],
                    b,
                    c
                ]
            ]]
        ]]
        )", 
        "SUM[USET[T2], FUN[k, T2, Times[b, c]]]");
}

TEST(DiracoqReduction, R_SUM_ELIM2) {
    TEST_RULE({R_SUM_ELIM2}, 
        R"(
            SUM[USET[T], FUN[i, T, 
                SCR[
                    DELTA[i, j],
                    A
                ]
            ]]
        )", 
        "A");
    TEST_RULE({R_SUM_ELIM2},
        R"(
        SUM[USET[T], FUN[i, T,
            SUM[USET[T2], FUN[k, T2, 
                SCR[
                    DELTA[i, j],
                    BRA[i]
                ]
            ]]
        ]]
        )", 
        "SUM[USET[T2], FUN[k, T2, BRA[j]]]");
}

TEST(DiracoqReduction, R_SUM_ELIM3) {
    TEST_RULE({R_SUM_ELIM3}, 
        R"(
            SUM[USET[T], FUN[i, T, 
                SCR[
                    Times[
                        DELTA[i, j], a, b
                    ],
                    A
                ]
            ]]
        )", 
        "SCR[Times[a, b], A]");
    TEST_RULE({R_SUM_ELIM3},
        R"(
        SUM[USET[T], FUN[i, T,
            SUM[USET[T2], FUN[k, T2, 
                SCR[
                    Times[DELTA[i, j], a],
                    BRA[i]
                ]
            ]]
        ]]
        )", 
        "SUM[USET[T2], FUN[k, T2, SCR[Times[a], BRA[j]]]]");
}

TEST(DiracoqReduction, R_SUM_ELIM4) {
    TEST_RULE({R_SUM_ELIM4},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                DELTA[i, j]
            ]]
        ]]
        )", 
        "SUM[M, FUN[j, T, 1]]");

    TEST_RULE({R_SUM_ELIM4},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                SUM[N, FUN[k, T,
                    DELTA[i, j]
                ]]
            ]]
        ]]
        )", 
        "SUM[M, FUN[j, T, SUM[N, FUN[k, T, 1]]]]");
}

TEST(DiracoqReduction, R_SUM_ELIM5) {
    TEST_RULE({R_SUM_ELIM5},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                Times[a, DELTA[i, j], b]
            ]]
        ]]
        )", 
        "SUM[M, FUN[j, T, Times[a, b]]]");
    
    TEST_RULE({R_SUM_ELIM5},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                SUM[N, FUN[k, T,
                    Times[a, DELTA[i, j], b]
                ]]
            ]]
        ]]
        )", 
        "SUM[M, FUN[j, T, SUM[N, FUN[k, T, Times[a, b]]]]]");
}

TEST(DiracoqReduction, R_SUM_ELIM6) {
    TEST_RULE({R_SUM_ELIM6},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                SCR[
                    DELTA[i, j],
                    A
                ]
            ]]
        ]]
        )", 
        "SUM[M, FUN[j, T, A]]");

    TEST_RULE({R_SUM_ELIM6},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                SCR[
                    DELTA[j, i],
                    KET[i]
                ]
            ]]
        ]]
        )", 
        "SUM[M, FUN[j, T, KET[j]]]");

    TEST_RULE({R_SUM_ELIM6},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                SUM[N, FUN[k, T,
                    SCR[
                        DELTA[i, j],
                        A
                    ]
                ]]
            ]]
        ]]
        )", 
        "SUM[M, FUN[j, T, SUM[N, FUN[k, T, A]]]]");
}

TEST(DiracoqReduction, R_SUM_ELIM7) {
    TEST_RULE({R_SUM_ELIM7},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                SCR[
                    Times[a, DELTA[i, j], b],
                    A
                ]
            ]]
        ]]
        )", 
        "SUM[M, FUN[j, T, SCR[Times[a, b], A]]]");
    
    TEST_RULE({R_SUM_ELIM7},
        R"(
        SUM[M, FUN[i, T,
            SUM[M, FUN[j, T, 
                SUM[N, FUN[k, T,
                    SCR[
                        Times[a, DELTA[i, j], b],
                        KET[j]
                    ]
                ]]
            ]]
        ]]
        )", 
        R"(
        SUM[M, FUN[j, T, 
            SUM[N, FUN[k, T, 
                SCR[
                    Times[a, b], 
                    KET[j]
                ]
            ]]
        ]]
        )");
}

TEST(DiracoqReduction, R_SUM_PUSH0) {
    TEST_RULE({R_SUM_PUSH0},
        R"(
        Times[
            a, b, c,
            SUM[M, FUN[i, T, d]],
            d, e
        ]
        )", 
        "SUM[M, FUN[i, T, Times[a, b, c, d, d, e]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH1) {
    TEST_RULE({R_SUM_PUSH1}, "Conjugate[SUM[M, FUN[i, T, a]]]", "SUM[M, FUN[i, T, Conjugate[a]]]");
}
TEST(DiracoqReduction, R_SUM_PUSH2) {
    TEST_RULE({R_SUM_PUSH2}, "ADJ[SUM[M, FUN[i, T, X]]]", "SUM[M, FUN[i, T, ADJ[X]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH3) {
    TEST_RULE({R_SUM_PUSH3}, "SCR[a, SUM[M, FUN[i, T, X]]]", "SUM[M, FUN[i, T, SCR[a, X]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH4) {
    TEST_RULE({R_SUM_PUSH4}, "SCR[SUM[M, FUN[i, T, a]], X]", "SUM[M, FUN[i, T, SCR[a, X]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH5) {
    TEST_RULE({R_SUM_PUSH5}, "DOT[SUM[M, FUN[i, T, B]], K]", "SUM[M, FUN[i, T, DOT[B, K]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH6) {
    TEST_RULE({R_SUM_PUSH6}, "MULK[SUM[M, FUN[i, T, O]], K]", "SUM[M, FUN[i, T, MULK[O, K]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH7) {
    TEST_RULE({R_SUM_PUSH7}, "MULB[SUM[M, FUN[i, T, B]], O]", "SUM[M, FUN[i, T, MULB[B, O]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH8) {
    TEST_RULE({R_SUM_PUSH8}, "OUTER[SUM[M, FUN[i, T, K]], B]", "SUM[M, FUN[i, T, OUTER[K, B]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH9) {
    TEST_RULE({R_SUM_PUSH9}, "MULO[SUM[M, FUN[i, T, O1]], O2]", "SUM[M, FUN[i, T, MULO[O1, O2]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH10) {
    TEST_RULE({R_SUM_PUSH10}, "DOT[B, SUM[M, FUN[i, T, K]]]", "SUM[M, FUN[i, T, DOT[B, K]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH11) {
    TEST_RULE({R_SUM_PUSH11}, "MULK[O, SUM[M, FUN[i, T, K]]]", "SUM[M, FUN[i, T, MULK[O, K]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH12) {
    TEST_RULE({R_SUM_PUSH12}, "MULB[B, SUM[M, FUN[i, T, O]]]", "SUM[M, FUN[i, T, MULB[B, O]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH13) {
    TEST_RULE({R_SUM_PUSH13}, "OUTER[K, SUM[M, FUN[i, T, B]]]", "SUM[M, FUN[i, T, OUTER[K, B]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH14) {
    TEST_RULE({R_SUM_PUSH14}, "MULO[O1, SUM[M, FUN[i, T, O2]]]", "SUM[M, FUN[i, T, MULO[O1, O2]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH15) {
    TEST_RULE({R_SUM_PUSH15}, "TSR[SUM[M, FUN[i, T, X]], Y]", "SUM[M, FUN[i, T, TSR[X, Y]]]");
}

TEST(DiracoqReduction, R_SUM_PUSH16) {
    TEST_RULE({R_SUM_PUSH16}, "TSR[X, SUM[M, FUN[i, T, Y]]]", "SUM[M, FUN[i, T, TSR[X, Y]]]");
}

TEST(DiracoqReduction, R_SUM_ADDS0) {
    TEST_RULE({R_SUM_ADDS0}, "SUM[M, FUN[i, T, Plus[a, b]]]", "Plus[SUM[M, FUN[$0, T, a]], SUM[M, FUN[$1, T, b]]]");
}

TEST(DiracoqReduction, R_SUM_ADD0) {
    TEST_RULE({R_SUM_ADD0}, "SUM[M, FUN[i, T, ADD[X, Y]]]", "ADD[SUM[M, FUN[$0, T, X]], SUM[M, FUN[$1, T, Y]]]");
}

TEST(DiracoqReduction, R_SUM_ADD1) {
    TEST_RULE({R_SUM_ADD1}, "SUM[M, FUN[i, T, SCR[Plus[a, b, c], KET[i]]]]", "ADD[SUM[M, FUN[$0, T, SCR[a, KET[$0]]]], SUM[M, FUN[$1, T, SCR[b, KET[$1]]]], SUM[M, FUN[$2, T, SCR[c, KET[$2]]]]]");
}

TEST(DiracoqReduction, R_SUM_INDEX0) {
    TEST_RULE({R_SUM_INDEX0}, "SUM[USET[PROD[T1, T2]], FUN[i, BASIS[PROD[T1, T2]], X]]", "SUM[USET[T1], FUN[$0, BASIS[T1], SUM[USET[T2], FUN[$1, BASIS[T2], X]]]]");
}

TEST(DiracoqReduction, R_SUM_INDEX1) {
    TEST_RULE({R_SUM_INDEX1}, "SUM[CATPROD[M1, M2], FUN[i, BASIS[PROD[T1, T2]], X]]", "SUM[M1, FUN[$0, BASIS[T1], SUM[M2, FUN[$1, BASIS[T2], X]]]]");
}

TEST(DiracoqReduction, R_QBIT_ONEO) {
    TEST_RULE({R_QBIT_ONEO}, "1O[QBIT]", "ADD[OUTER[KET[#0], BRA[#0]], OUTER[KET[#1], BRA[#1]]]");
}

TEST(DiracoqReduction, R_QBIT_ZEROO) {
    TEST_RULE({R_QBIT_SUM}, "SUM[USET[QBIT], FUN[i, BASIS[QBIT], OUTER[KET[i], BRA[i]]]]", "ADD[OUTER[KET[#0], BRA[#0]], OUTER[KET[#1], BRA[#1]]]");
}

TEST(DiracoqReduction, R_LABEL_EXPAND_K) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("r"), kernel.parse("REG[T1]"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T1]"));

    TEST_RULE(kernel, {R_LABEL_EXPAND}, "SUBS[K, r]", "SUM[USET[T1], FUN[$0, BASIS[T1], SCR[DOT[BRA[$0], K], LTSR[LKET[$0, r]]]]]");
}


TEST(DiracoqReduction, R_LABEL_EXPAND_B) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("r"), kernel.parse("REG[T1]"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T1]"));

    TEST_RULE(kernel, {R_LABEL_EXPAND}, "SUBS[B, r]", "SUM[USET[T1], FUN[$0, BASIS[T1], SCR[DOT[B, KET[$0]], LTSR[LBRA[$0, r]]]]]");
}



TEST(DiracoqReduction, R_LABEL_EXPAND_O) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("r"), kernel.parse("REG[T1]"));
    kernel.assum(kernel.register_symbol("O"), kernel.parse("OTYPE[T1, T1]"));

    TEST_RULE(kernel, {R_LABEL_EXPAND}, "SUBS[O, r, r]", "SUM[USET[T1], FUN[$0, BASIS[T1], SUM[USET[T1], FUN[$1, BASIS[T1], SCR[DOT[BRA[$0], MULK[O, KET[$1]]], LDOT[LTSR[LKET[$0, r]], LTSR[LBRA[$1, r]]]]]]]]");
}

TEST(DiracoqReduction, R_ADJD0) {
    TEST_RULE({R_ADJD0}, "ADJ[LTSR[X1, X2, X3]]", "LTSR[ADJ[X3], ADJ[X2], ADJ[X1]]");
}

TEST(DiracoqReduction, R_ADJD1) {
    TEST_RULE({R_ADJD1}, "ADJ[LDOT[X1, X2]]", "LDOT[ADJ[X2], ADJ[X1]]");
}

TEST(DiracoqReduction, R_SCRD0) {
    TEST_RULE({R_SCRD0}, "LTSR[SCR[a, X1], X2, X3]", "SCR[a, LTSR[X1, X2, X3]]");
}

TEST(DiracoqReduction, R_SCRD1) {
    TEST_RULE({R_SCRD1}, "LDOT[SCR[a, X1], X2]", "SCR[a, LDOT[X1, X2]]");
}

TEST(DiracoqReduction, R_SCRD2) {
    TEST_RULE({R_SCRD2}, "LDOT[X1, SCR[a, X2]]", "SCR[a, LDOT[X1, X2]]");
}

TEST(DiracoqReduction, R_TSRD0) {
    TEST_RULE({R_TSRD0}, "LTSR[X1, ADD[X2, X3], X4]", "ADD[LTSR[X1, X2, X4], LTSR[X1, X3, X4]]");
}

TEST(DiracoqReduction, R_DOTD0) {
    TEST_RULE({R_DOTD0}, "LDOT[ADD[X1, X2], X3]", "ADD[LDOT[X1, X3], LDOT[X2, X3]]");
}

TEST(DiracoqReduction, R_DOTD1) {
    TEST_RULE({R_DOTD1}, "LDOT[X1, ADD[X2, X3]]", "ADD[LDOT[X1, X2], LDOT[X1, X3]]");
}

TEST(DiracoqReduction, R_SUM_PUSHD0) {
    TEST_RULE({R_SUM_PUSHD0}, "LTSR[X1, SUM[M, FUN[i, BASIS[T], X2]], X3]", "SUM[M, FUN[i, BASIS[T], LTSR[X1, X2, X3]]]"); 
}

TEST(DiracoqReduction, R_SUM_PUSHD1) {
    TEST_RULE({R_SUM_PUSHD1}, "LDOT[SUM[M, FUN[i, BASIS[T], X1]], X2]", "SUM[M, FUN[i, BASIS[T], LDOT[X1, X2]]]"); 
}

TEST(DiracoqReduction, R_SUM_PUSHD2) {
    TEST_RULE({R_SUM_PUSHD2}, "LDOT[X1, SUM[M, FUN[i, BASIS[T], X2]]]", "SUM[M, FUN[i, BASIS[T], LDOT[X1, X2]]]");
}

TEST(DiracoqReduction, R_L_SORT0) {
    Kernel kernel;
    kernel.assum(kernel.register_symbol("T1"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("T2"), kernel.parse("INDEX"));
    kernel.assum(kernel.register_symbol("r1"), kernel.parse("REG[T1]"));
    kernel.assum(kernel.register_symbol("r2"), kernel.parse("REG[T2]"));
    kernel.assum(kernel.register_symbol("K"), kernel.parse("KTYPE[T1]"));
    kernel.assum(kernel.register_symbol("B"), kernel.parse("BTYPE[T2]"));
    
    TEST_RULE(kernel, {R_L_SORT0}, "LDOT[SUBS[K, r1], SUBS[B, r2]]", "LTSR[SUBS[K, r1], SUBS[B, r2]]");
}

TEST(DiracoqReduction, R_L_SORT1) {
    TEST_RULE({R_L_SORT1}, "LDOT[LBRA[i, r], LKET[j, r]]", "DELTA[i, j]");
}

TEST(DiracoqReduction, R_L_SORT2) {
    TEST_RULE({R_L_SORT2}, "LDOT[LBRA[i, r], LTSR[X1, X2, LKET[j, r], X3]]", "SCR[DELTA[i, j], LTSR[X1, X2, X3]]");
}

TEST(DiracoqReduction, R_L_SORT3) {
    TEST_RULE({R_L_SORT3}, "LDOT[LTSR[X1, X2, LBRA[i, r], X3], LKET[j, r]]", "SCR[DELTA[i, j], LTSR[X1, X2, X3]]");
}

TEST(DiracoqReduction, R_L_SORT4) {
    TEST_RULE({R_L_SORT4}, 
        "LDOT[LTSR[X1, X2, LBRA[i, r]], LTSR[Y1, LKET[j, r], Y2]]", 
        "SCR[DELTA[i, j], LDOT[LTSR[X1, X2], LTSR[Y1, Y2]]]"); 
}

// ///////////////////////////////////////////////////////
// // Combined Tests

// // (a + (b * 0))^* -> 0
// TEST(DiracoqReduction, Combined1) {
//     TEST_RULE(rules, "Conjugate(Plus(a Times(b 0)))", "Conjugate(a)");
// }

// // (b * 0)^*^* -> 0
// TEST(DiracoqReduction, Combined2) {
//     TEST_RULE(rules, "Conjugate(Conjugate(Times(b 0)))", "0");
// }
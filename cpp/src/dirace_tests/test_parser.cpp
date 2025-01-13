#include <gtest/gtest.h>

#include "dirace.hpp"

using namespace ualg;
using namespace std;
using namespace dirace;

TEST(DiraceParser, Definition0) {
    auto actual_res = parse("Def a := x.");
    auto expected_res = astparser::parse("GROUP[DEF[a, x]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Definition1) {
    auto actual_res = parse("Def a := x : type.");
    auto expected_res = astparser::parse("GROUP[DEF[a, x, type]]");
    EXPECT_EQ(actual_res, expected_res);
}


TEST(DiraceParser, Assum) {
    auto actual_res = parse("Var a : type.");
    auto expected_res = astparser::parse("GROUP[VAR[a, type]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Check) {
    auto actual_res = parse("Check a.");
    auto expected_res = astparser::parse("GROUP[CHECK[a]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Show) {
    auto actual_res = parse("Show a.");
    auto expected_res = astparser::parse("GROUP[SHOW[a]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, ShowAll) {
    auto actual_res = parse("ShowAll.");
    auto expected_res = astparser::parse("GROUP[SHOWALL]");
    EXPECT_EQ(actual_res, expected_res);
}

// An extra test for grouping
TEST(DiraceParser, CmdSeq) {
    auto actual_res = parse("Var a : TYPE. Var b : TYPE. Check a. Check b.");
    auto expected_res = astparser::parse("GROUP[VAR[a, TYPE], VAR[b, TYPE], CHECK[a], CHECK[b]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Normalize) {
    auto actual_res = parse("Normalize a.");
    auto expected_res = astparser::parse("GROUP[NORMALIZE[a]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, NormalizeTraced) {
    auto actual_res = parse("Normalize a with trace.");
    auto expected_res = astparser::parse("GROUP[NORMALIZE[a, TRACE]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, CheckEq) {
    auto actual_res = parse("CheckEq a with b.", true);
    auto expected_res = astparser::parse("GROUP[CHECKEQ[a, b]]");
    cout << actual_res->to_string() << endl;
    cout << expected_res->to_string() << endl;
    EXPECT_EQ(actual_res, expected_res);
}


////////////////////////////////////////////////////
// term


TEST(DiraceParser, FORALL) {
    auto actual_res = parse("forall x. T");
    auto expected_res = astparser::parse("FORALL[x, T]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Fun) {
    auto actual_res = parse("fun x : T => (x, x)");
    auto expected_res = astparser::parse("FUN[x, T, PAIR[x, x]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Idx) {
    auto actual_res = parse("idx sigma => 0K[sigma]");
    auto expected_res = astparser::parse("IDX[sigma, ZEROK[sigma]]");
    cout << actual_res->to_string() << endl;
    cout << expected_res->to_string() << endl;
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Sum) {
    auto actual_res = parse("Sum x in S, x x");
    auto expected_res = astparser::parse("SSUM[x, S, COMPO[x, x]]");
    cout << actual_res->to_string() << endl;
    cout << expected_res->to_string() << endl;
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, ARROW) {
    auto actual_res = parse("T1 -> T2");
    auto expected_res = astparser::parse("ARROW[T1, T2]");
    EXPECT_EQ(actual_res, expected_res);

    actual_res = parse("T1 -> T2 -> T3");
    expected_res = astparser::parse("ARROW[T1, ARROW[T2, T3]]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Add) {
    auto actual_res = parse("a + b");
    auto expected_res = astparser::parse("ADDG[a, b]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Compo) {
    auto actual_res = parse("a b");
    auto expected_res = astparser::parse("COMPO[a, b]");
    EXPECT_EQ(actual_res, expected_res);

    actual_res = parse("a b c");
    expected_res = astparser::parse("COMPO[COMPO[a, b], c]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Star) {
    auto actual_res = parse("a * b");
    auto expected_res = astparser::parse("STAR[a, b]");
    EXPECT_EQ(actual_res, expected_res);

    actual_res = parse("a * b * c * d");
    expected_res = astparser::parse("STAR[STAR[STAR[a, b], c], d]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Conj) {
    auto actual_res = parse("a^*");
    auto expected_res = astparser::parse("Conjugate[a]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Adj) {
    auto actual_res = parse("a^D");
    auto expected_res = astparser::parse("ADJ[a]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Scr) {
    auto actual_res = parse("a . b");
    auto expected_res = astparser::parse("SCR[a, b]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Pair) {
    auto actual_res = parse("(T1, T2)");
    auto expected_res = astparser::parse("PAIR[T1, T2]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Delta) {
    auto actual_res = parse("delta(T1, T2)");
    auto expected_res = astparser::parse("DELTA[T1, T2]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Ket) {
    auto actual_res = parse("|a>");
    auto expected_res = astparser::parse("KET[a]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Bra) {
    auto actual_res = parse("<a|");
    auto expected_res = astparser::parse("BRA[a]");
    EXPECT_EQ(actual_res, expected_res);
}


TEST(DiraceParser, Paren) {
    auto actual_res = parse("(T1 -> T2) -> T3");
    auto expected_res = astparser::parse("ARROW[ARROW[T1, T2], T3]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, Application) {
    auto actual_res = parse("A[B, C]");
    auto expected_res = astparser::parse("A[B, C]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, RSet) {
    auto actual_res = parse("{a, b, c}");
    auto expected_res = astparser::parse("RSET[a, b, c]");
    EXPECT_EQ(actual_res, expected_res);

    actual_res = parse("{}");
    expected_res = astparser::parse("RSET[]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, SUBS1) {
    auto actual_res = parse("K_r");
    auto expected_res = astparser::parse("SUBS[K, r]");
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiraceParser, SUBS2) {
    auto actual_res = parse("K_r;s");
    auto expected_res = astparser::parse("SUBS[K, r, s]");
    EXPECT_EQ(actual_res, expected_res);
}

////////////////////////////////////////////////////
// Check Precedence
TEST(DiraceParser, precedence1) {
    auto actual_res = parse("a + b c");
    auto expected_res = astparser::parse("ADDG[a, COMPO[b, c]]");
    EXPECT_EQ(actual_res, expected_res);
}
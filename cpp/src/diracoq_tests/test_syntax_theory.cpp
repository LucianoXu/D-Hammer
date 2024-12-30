#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;


TEST(DiracoqSyntaxTheory, FreeIn1) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "APPLY[x, y]");
    auto var = parse(sig, bank, "x")->get_head();
    EXPECT_FALSE(free_in(term, var));
}

TEST(DiracoqSyntaxTheory, FreeIn2) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "APPLY[x, y]");
    auto var = parse(sig, bank, "z")->get_head();
    EXPECT_TRUE(free_in(term, var));
}

TEST(DiracoqSyntaxTheory, FreeIn3) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "FUN[x, T, APPLY[x, y]]");
    auto var = parse(sig, bank, "x")->get_head();
    EXPECT_TRUE(free_in(term, var));
}


TEST(DiracoqSyntaxTheory, FreeIn4) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "FUN[x, KTYPE[x], APPLY[x, y]]");
    auto var = parse(sig, bank, "x")->get_head();
    EXPECT_FALSE(free_in(term, var));
}

TEST(DiracoqSyntaxTheory, Substitution1) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto initial_term = parse(sig, bank, "IDX[x, T]]");
    auto var = parse(sig, bank, "x")->get_head();
    
    auto actual_res = subst(sig, bank, initial_term, var, parse(sig, bank, "z"));
    auto expected_res = initial_term;

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqSyntaxTheory, Substitution2) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto initial_term = parse(sig, bank, "IDX[x, T]]");
    auto var = parse(sig, bank, "T")->get_head();
    
    auto actual_res = subst(sig, bank, initial_term, var, parse(sig, bank, "APPLY[x, x]"));
    auto expected_res = parse(sig, bank, "IDX[@0, APPLY[x, x]]]");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqSyntaxTheory, Substitution3) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto initial_term = parse(sig, bank, "FUN[x, KTYPE[x], APPLY[x, y]]");
    auto var = parse(sig, bank, "x")->get_head();
    
    auto actual_res = subst(sig, bank, initial_term, var, parse(sig, bank, "y"));
    auto expected_res = parse(sig, bank, "FUN[x, KTYPE[y], APPLY[x, y]]");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqSyntaxTheory, alpha_eq1) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term1 = parse(sig, bank, "FUN[x, KTYPE[x], APPLY[x, y]]");
    auto term2 = parse(sig, bank, "FUN[z, KTYPE[x], APPLY[z, y]]");

    EXPECT_TRUE(alpha_eq(sig, bank, term1, term2));
}

TEST(DiracoqSyntaxTheory, to_deBruijn1) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "FUN[x, KTYPE[x], APPLY[x, y]]");
    auto actual_res = to_deBruijn(sig, bank, term);
    auto expected_res = parse(sig, bank, "FUN[KTYPE[x], APPLY[$0, y]]");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqSyntaxTheory, to_deBruijn2) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "FUN[x, KTYPE[x], APPLY[x, FUN[y, T, APPLY[y, x]]]]");
    auto actual_res = to_deBruijn(sig, bank, term);
    auto expected_res = parse(sig, bank, "FUN[KTYPE[x], APPLY[$0, FUN[T, APPLY[$0, $1]]]]");

    EXPECT_EQ(actual_res, expected_res);
}
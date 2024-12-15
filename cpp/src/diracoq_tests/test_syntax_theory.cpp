#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;


TEST(DiracoqSyntaxTheory, FreeIn1) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "apply(x y)");
    auto var = parse(sig, bank, "x")->get_head();
    EXPECT_FALSE(free_in(term, var));
}

TEST(DiracoqSyntaxTheory, FreeIn2) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "apply(x y)");
    auto var = parse(sig, bank, "z")->get_head();
    EXPECT_TRUE(free_in(term, var));
}

TEST(DiracoqSyntaxTheory, FreeIn3) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "fun(x T apply(x y))");
    auto var = parse(sig, bank, "x")->get_head();
    EXPECT_TRUE(free_in(term, var));
}


TEST(DiracoqSyntaxTheory, FreeIn4) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term = parse(sig, bank, "fun(x KType(x) apply(x y))");
    auto var = parse(sig, bank, "x")->get_head();
    EXPECT_FALSE(free_in(term, var));
}

TEST(DiracoqSyntaxTheory, Substitution1) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto initial_term = parse(sig, bank, "idx(x T))");
    auto var = parse(sig, bank, "x")->get_head();
    
    auto actual_res = subst(sig, bank, initial_term, var, parse(sig, bank, "z"));
    auto expected_res = initial_term;

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqSyntaxTheory, Substitution2) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto initial_term = parse(sig, bank, "idx(x T))");
    auto var = parse(sig, bank, "T")->get_head();
    
    auto actual_res = subst(sig, bank, initial_term, var, parse(sig, bank, "apply(x x)"));
    auto expected_res = parse(sig, bank, "idx(@0 apply(x x)))");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqSyntaxTheory, Substitution3) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto initial_term = parse(sig, bank, "fun(x KType(x) apply(x y))");
    auto var = parse(sig, bank, "x")->get_head();
    
    auto actual_res = subst(sig, bank, initial_term, var, parse(sig, bank, "y"));
    auto expected_res = parse(sig, bank, "fun(x KType(y) apply(x y))");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqSyntaxTheory, alpha_eq1) {
    TermBank<int> bank;
    auto sig = diracoq_sig;

    auto term1 = parse(sig, bank, "fun(x KType(x) apply(x y))");
    auto term2 = parse(sig, bank, "fun(z KType(x) apply(z y))");

    EXPECT_TRUE(alpha_eq(sig, bank, term1, term2));
}
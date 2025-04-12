#include <gtest/gtest.h>

#include "dhammer.hpp"

using namespace ualg;
using namespace std;
using namespace dhammer;


TEST(dhammerSyntaxTheory, FreeIn1) {
    auto sig = dhammer_sig;

    auto term = sig.parse("APPLY[x, y]");
    auto var = sig.parse("x")->get_head();
    EXPECT_FALSE(free_in(term, var));
}

TEST(dhammerSyntaxTheory, FreeIn2) {
    auto sig = dhammer_sig;

    auto term = sig.parse("APPLY[x, y]");
    auto var = sig.parse("z")->get_head();
    EXPECT_TRUE(free_in(term, var));
}

TEST(dhammerSyntaxTheory, FreeIn3) {
    auto sig = dhammer_sig;

    auto term = sig.parse("FUN[x, T, APPLY[x, y]]");
    auto var = sig.parse("x")->get_head();
    EXPECT_TRUE(free_in(term, var));
}


TEST(dhammerSyntaxTheory, FreeIn4) {
    auto sig = dhammer_sig;

    auto term = sig.parse("FUN[x, KTYPE[x], APPLY[x, y]]");
    auto var = sig.parse("x")->get_head();
    EXPECT_FALSE(free_in(term, var));
}

TEST(dhammerSyntaxTheory, Substitution1) {
    auto sig = dhammer_sig;

    auto initial_term = sig.parse("IDX[x, T]]");
    auto var = sig.parse("x")->get_head();

    auto actual_res = subst(sig, initial_term, var, sig.parse("z"));
    auto expected_res = initial_term;

    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(dhammerSyntaxTheory, Substitution2) {
    auto sig = dhammer_sig;

    auto initial_term = sig.parse("IDX[x, T]]");
    auto var = sig.parse("T")->get_head();

    auto actual_res = subst(sig, initial_term, var, sig.parse("APPLY[x, x]"));
    auto expected_res = sig.parse("IDX[$0, APPLY[x, x]]]");

    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(dhammerSyntaxTheory, Substitution3) {
    auto sig = dhammer_sig;

    auto initial_term = sig.parse("FUN[x, KTYPE[x], APPLY[x, y]]");
    auto var = sig.parse("x")->get_head();

    auto actual_res = subst(sig, initial_term, var, sig.parse("y"));
    auto expected_res = sig.parse("FUN[x, KTYPE[y], APPLY[x, y]]");

    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(dhammerSyntaxTheory, to_deBruijn1) {
    auto sig = dhammer_sig;

    auto term = sig.parse("FUN[x, KTYPE[x], APPLY[x, y]]");
    auto actual_res = to_deBruijn(sig, term);
    auto expected_res = sig.parse("FUN[KTYPE[x], APPLY[$0, y]]");

    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(dhammerSyntaxTheory, to_deBruijn2) {
    auto sig = dhammer_sig;

    auto term = sig.parse("FUN[x, KTYPE[x], APPLY[x, FUN[y, T, APPLY[y, x]]]]");
    auto actual_res = to_deBruijn(sig, term);
    auto expected_res = sig.parse("FUN[KTYPE[x], APPLY[$0, FUN[T, APPLY[$0, $1]]]]");

    EXPECT_EQ(*actual_res, *expected_res);
}
#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;


TEST(DiracoqReduction, Beta) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("T"));

    auto term = kernel.parse("apply(fun(x T x) a)");
    auto normal_term = static_cast<const NormalTerm<int>*>(term);

    auto actual_res = pos_rewrite_repeated(kernel, normal_term, rules);
    auto expected_res = kernel.parse("a");

    EXPECT_EQ(actual_res, expected_res);
}


TEST(DiracoqReduction, default_searcher) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("T"));
    kernel.def(kernel.register_symbol("f"), kernel.parse("fun(x T x)"));

    auto term = kernel.parse("apply(f a)");
    auto normal_term = static_cast<const NormalTerm<int>*>(term);

    auto actual_res = pos_rewrite_repeated(kernel, normal_term, rules);
    auto expected_res = kernel.parse("a");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqReduction, Eta) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("Arrow(T T)"));

    auto term = kernel.parse("fun(x T apply(f x))");
    auto normal_term = static_cast<const NormalTerm<int>*>(term);

    auto actual_res = pos_rewrite_repeated(kernel, normal_term, rules);
    auto expected_res = kernel.parse("f");

    cout << kernel.term_to_string(actual_res) << endl;
    cout << kernel.term_to_string(expected_res) << endl;

    EXPECT_EQ(actual_res, expected_res);
}


/**
 * @brief The helper function for testing a single rewriting rule.
 * 
 * @param rules
 * @param variables 
 * @param input 
 * @param expected 
 */
void TEST_RULE(const vector<PosRewritingRule>& rules, string input, string expected) {
    Kernel kernel;

    auto term = static_cast<const NormalTerm<int>*>(kernel.parse(input));
    auto actual_res = pos_rewrite_repeated(kernel, term, rules);
    auto expected_res = static_cast<const NormalTerm<int>*>(kernel.parse(expected));
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqReduction, R_FLATTEN) {
    TEST_RULE({R_FLATTEN}, "MLTS(a MLTS(a b) b)", "MLTS(a a b b)");
    TEST_RULE({R_FLATTEN}, "MLTS(a ADDS(a b) b)", "MLTS(a ADDS(a b) b)");
}

TEST(DiracoqReduction, R_ADDS0) {
    TEST_RULE({R_ADDSID, R_ADDS0}, "ADDS(0 a b)", "ADDS(a b)");
    TEST_RULE({R_ADDSID, R_ADDS0}, "ADDS(0 a b 0 0)", "ADDS(a b)");
    TEST_RULE({R_ADDSID, R_ADDS0}, "ADDS(0 b)", "b");
    TEST_RULE({R_ADDS0}, "ADDS(0 0)", "ADDS(0)");
    TEST_RULE({R_ADDS0}, "ADDS(0)", "ADDS(0)");
}

TEST(DiracoqReduction, R_MLTS0) {
    TEST_RULE({R_MLTSID, R_MLTS0}, "MLTS(0 a b)", "0");
}

TEST(DiracoqReduction, R_MLTS1) {
    TEST_RULE({R_MLTSID, R_MLTS1}, "MLTS(1 a b)", "MLTS(a b)");
    TEST_RULE({R_MLTSID, R_MLTS1}, "MLTS(1 a 1 b 1)", "MLTS(a b)");
    TEST_RULE({R_MLTSID, R_MLTS1}, "MLTS(1 b)", "b");
    TEST_RULE({R_MLTS1}, "MLTS(1 1)", "MLTS(1)");
    TEST_RULE({R_MLTS1}, "MLTS(1)", "MLTS(1)");
}

TEST(DiracoqReduction, R_MLTS2) {
    TEST_RULE({R_MLTS2}, "MLTS(a ADDS(b c))", "ADDS(MLTS(a b) MLTS(a c))");
    TEST_RULE({R_MLTS2}, "MLTS(a ADDS(b c) b)", "ADDS(MLTS(a b b) MLTS(a c b))");
}

TEST(DiracoqReduction, R_MLTS2_nasty) {
    TEST_RULE({R_MLTS2}, "MLTS(ADDS(b c))", "MLTS(ADDS(b c))");
}

TEST(DiracoqReduction, R_CONJ0) {
    TEST_RULE({R_CONJ0}, "CONJ(0)", "0");
    TEST_RULE({R_CONJ0}, "ADDS(CONJ(0) a)", "ADDS(0 a)");
}

TEST(DiracoqReduction, R_CONJ1) {
    TEST_RULE({R_CONJ1}, "CONJ(1)", "1");
    TEST_RULE({R_CONJ1}, "ADDS(CONJ(1) a)", "ADDS(1 a)");
}

TEST(DiracoqReduction, R_CONJ2) {
    TEST_RULE({R_CONJ2}, "CONJ(ADDS(a b))", "ADDS(CONJ(a) CONJ(b))");
    TEST_RULE({R_CONJ2}, "CONJ(ADDS(a b c))", "ADDS(CONJ(a) CONJ(b) CONJ(c))");
}

TEST(DiracoqReduction, R_CONJ3) {
    TEST_RULE({R_CONJ3}, "CONJ(MLTS(a b))", "MLTS(CONJ(a) CONJ(b))");
    TEST_RULE({R_CONJ3}, "CONJ(MLTS(a b c))", "MLTS(CONJ(a) CONJ(b) CONJ(c))");
}

TEST(DiracoqReduction, R_CONJ4) {
    TEST_RULE({R_CONJ4}, "CONJ(CONJ(a))", "a");
    TEST_RULE({R_CONJ4}, "CONJ(CONJ(ADDS(a b)))", "ADDS(a b)");
}

///////////////////////////////////////////////////////
// Combined Tests

// (a + (b * 0))^* -> 0
TEST(DiracoqReduction, Combined1) {
    TEST_RULE(rules, "CONJ(ADDS(a MLTS(b 0)))", "CONJ(a)");
}

// (b * 0)^*^* -> 0
TEST(DiracoqReduction, Combined2) {
    TEST_RULE(rules, "CONJ(CONJ(MLTS(b 0)))", "0");
}
#include <gtest/gtest.h>

#include "scalar.hpp"
#include "ualg.hpp"

using namespace ualg;
using namespace std;
using namespace scalar;

/**
 * @brief The helper function for testing a single rewriting rule.
 * 
 * @param rules
 * @param variables 
 * @param input 
 * @param expected 
 */
void TEST_RULE(const vector<RewritingRule<int>>& rules, vector<string> variables, string input, string expected) {
    TermBank<int> bank{};
    Signature<int> sig = reserved_sig;
    for (const auto& var : variables) {
        sig.register_symbol(var);
    }

    auto term = parse(sig, bank, input);
    auto actual_res = rewrite_repeated(bank, term, rules);
    auto expected_res = parse(sig, bank, expected);
    EXPECT_EQ(actual_res, expected_res);
}



TEST(TestScalar, R_ADDS0) {
    TEST_RULE({R_ADDSID, R_ADDS0}, {"a", "b"}, "ADDS(0 a b)", "ADDS(a b)");
    TEST_RULE({R_ADDSID, R_ADDS0}, {"b"}, "ADDS(0 b)", "b");
}


TEST(TestScalar, R_MULS0) {
    TEST_RULE({R_MULSID, R_MULS0}, {"a", "b"}, "MULS(0 a b)", "0");
}

TEST(TestScalar, R_MULS1) {
    TEST_RULE({R_MULSID, R_MULS1}, {"a", "b"}, "MULS(1 a b)", "MULS(a b)");
    TEST_RULE({R_MULSID, R_MULS1}, {"b"}, "MULS(1 b)", "b");
}

TEST(TestScalar, R_MULS2) {
    TEST_RULE({R_MULS2}, {"a", "b", "c"}, "MULS(a ADDS(b c))", "ADDS(MULS(a b) MULS(a c))");
    TEST_RULE({R_MULS2}, {"a", "b", "c"}, "MULS(a ADDS(b c) b)", "ADDS(MULS(a b b) MULS(a b c))");
}

TEST(TestScalar, R_CONJ0) {
    TEST_RULE({R_CONJ0}, {}, "CONJ(0)", "0");
    TEST_RULE({R_CONJ0}, {"a"}, "ADDS(CONJ(0) a)", "ADDS(0 a)");
}

TEST(TestScalar, R_CONJ1) {
    TEST_RULE({R_CONJ1}, {}, "CONJ(1)", "1");
    TEST_RULE({R_CONJ1}, {"a"}, "ADDS(CONJ(1) a)", "ADDS(1 a)");
}

TEST(TestScalar, R_CONJ2) {
    TEST_RULE({R_CONJ2}, {"a", "b"}, "CONJ(ADDS(a b))", "ADDS(CONJ(a) CONJ(b))");
    TEST_RULE({R_CONJ2}, {"a", "b", "c"}, "CONJ(ADDS(a b c))", "ADDS(CONJ(a) CONJ(b) CONJ(c))");
}

TEST(TestScalar, R_CONJ3) {
    TEST_RULE({R_CONJ3}, {"a", "b"}, "CONJ(MULS(a b))", "MULS(CONJ(a) CONJ(b))");
    TEST_RULE({R_CONJ3}, {"a", "b", "c"}, "CONJ(MULS(a b c))", "MULS(CONJ(a) CONJ(b) CONJ(c))");
}

TEST(TestScalar, R_CONJ4) {
    TEST_RULE({R_CONJ4}, {"a"}, "CONJ(CONJ(a))", "a");
    TEST_RULE({R_CONJ4}, {"a", "b"}, "CONJ(CONJ(ADDS(a b)))", "ADDS(a b)");
}

///////////////////////////////////////////////////////
// Combined Tests

// (a + (b * 0))^* -> 0
TEST(TestScalar, Combined1) {
    TEST_RULE(scalar_rules, {"a", "b"}, "CONJ(ADDS(a MULS(b 0)))", "CONJ(a)");
}

// (b * 0)^*^* -> 0
TEST(TestScalar, Combined2) {
    TEST_RULE(scalar_rules, {"a", "b"}, "CONJ(CONJ(MULS(b 0)))", "0");
}
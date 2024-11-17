#include <gtest/gtest.h>

#include "scalar.hpp"
#include "ualg.hpp"

using namespace ualg;
using namespace std;
using namespace scalar;

/**
 * @brief The helper function for testing a single rewriting rule.
 * 
 * @tparam T 
 * @param rule 
 * @param variables 
 * @param input 
 * @param expected 
 */
template <class T>
void SINGLE_RULE_TEST(RewritingRule<T> rule, vector<string> variables, string input, string expected) {
    TermBank<int> bank{};
    StringSymbolType variable_symbols;
    for (const auto& var : variables) {
        variable_symbols.push_back({var, SymbolType::NORMAL});
    }

    Signature<int> sig = compile_string_sig(
        extend_string_symbol_list(
        symbols,
        variable_symbols)
    );
    auto term = parse(sig, bank, input);
    auto actual_res = rewrite_repeated(bank, term, {rule});
    auto expected_res = parse(sig, bank, expected);
    EXPECT_EQ(actual_res, expected_res);
}



TEST(TestScalar, R_ADDS0) {
    SINGLE_RULE_TEST(R_ADDS0, {"a", "b"}, "ADDS(0 a b)", "ADDS(a b)");
    SINGLE_RULE_TEST(R_ADDS0, {"b"}, "ADDS(0 b)", "b");
}


TEST(TestScalar, R_MLTS0) {
    SINGLE_RULE_TEST(R_MLTS0, {"a", "b"}, "MLTS(0 a b)", "0");
}

TEST(TestScalar, R_MLTS1) {
    SINGLE_RULE_TEST(R_MLTS1, {"a", "b"}, "MLTS(1 a b)", "MLTS(a b)");
    SINGLE_RULE_TEST(R_MLTS1, {"b"}, "MLTS(1 b)", "b");
}

TEST(TestScalar, R_MLTS2) {
    SINGLE_RULE_TEST(R_MLTS2, {"a", "b", "c"}, "MLTS(a ADDS(b c))", "ADDS(MLTS(a b) MLTS(a c))");
    SINGLE_RULE_TEST(R_MLTS2, {"a", "b", "c"}, "MLTS(a ADDS(b c) b)", "ADDS(MLTS(a b b) MLTS(a b c))");
}


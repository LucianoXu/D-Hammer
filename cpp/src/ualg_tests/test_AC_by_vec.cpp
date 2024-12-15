
#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;


TEST(TestACflatten, flatten) {
    TermBank<string> bank{};
    Signature<string> sig = {
        {{"f", "f"}, {"a", "a"}, {"b", "b"}},
    };

    auto t = parse(sig, bank, "f(a f(a b) b)");

    auto actual_res = flatten(t, bank, {"f"});
    auto expected_res = parse(sig, bank, "f(a a b b)");

    EXPECT_EQ(actual_res, expected_res);
}


//////////////////////////////////////
// AC-theory by normal terms + sorting
TEST(TestCProofInstruct, to_string) {
    CProofInstruct instruct{
        {
            {0, CProofInstruct{}},
            {2, CProofInstruct{}},
            {1, CProofInstruct{}}
        }
    };

    EXPECT_EQ(instruct.to_string(), "[0:E 2:E 1:E]");
}

TEST(TestCProofInstruct, apply_forward) {
    TermBank<string> bank{};
    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}, {"a", "a"}, {"b", "b"}, {"c", "c"}}
    };

    string input = "f(a f(a c b))";

    auto initial_term = parse(sig, bank, input);

    auto [sorted_term, sorted_instruct] = sort_CInstruct(initial_term, bank, {"f", "g"});

    auto actual_res = apply_CInstruct(initial_term, sorted_instruct, bank);
    auto expected_res = sorted_term;

    EXPECT_EQ(actual_res, expected_res);
}

TEST(TestCProofInstruct, inverse_apply) {
    
    TermBank<string> bank{};
    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}, {"a", "a"}, {"b", "b"}, {"c", "c"}}
    };

    string input = "f(a g(b g(a c b)) f(a b))";

    auto initial_term = parse(sig, bank, input);

    auto [sorted_term, sorted_instruct] = sort_CInstruct(initial_term, bank, {"f", "g"});

    auto inverse_instruct = sorted_instruct.inverse();

    auto actual_res = apply_CInstruct(sorted_term, inverse_instruct, bank);
    auto expected_res = initial_term;
    
    EXPECT_EQ(actual_res, expected_res);
}


TEST(TestCProofInstruct, compose_inverse1) {
    
    TermBank<string> bank{};
    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}, {"a", "a"}, {"b", "b"}, {"c", "c"}}
    };

    string input = "f(a g(b g(a c b)) f(a b))";

    auto initial_term = parse(sig, bank, input);

    auto [sorted_term, sorted_instruct] = sort_CInstruct(initial_term, bank, {"f", "g"});

    auto inverse_instruct = sorted_instruct.inverse();
    auto compose_instruct = sorted_instruct.compose(inverse_instruct);

    auto actual_res = apply_CInstruct(initial_term, compose_instruct, bank);
    auto expected_res = initial_term;
    
    EXPECT_EQ(actual_res, expected_res);
}

TEST(TestCProofInstruct, compose_inverse2) {
    
    TermBank<string> bank{};
    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}, {"a", "a"}, {"b", "b"}, {"c", "c"}}
    };

    string input = "f(g(b a) g(b g(a c b) a) f(f(b)))";

    auto initial_term = parse(sig, bank, input);

    auto [sorted_term, sorted_instruct] = sort_CInstruct(initial_term, bank, {"f", "g"});

    auto inverse_instruct = sorted_instruct.inverse();
    auto compose_instruct = sorted_instruct.compose(inverse_instruct);

    auto actual_res = apply_CInstruct(initial_term, compose_instruct, bank);
    auto expected_res = initial_term;
    
    EXPECT_EQ(actual_res, expected_res);
}

TEST(TestCProofInstruct, check_C_eq1) {

    TermBank<string> bank{};
    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}, {"a", "a"}, {"b", "b"}, {"c", "c"}}
    };

    string inputA = "f(a g(b g(a c b)) f(a b))";
    string inputB = "f(g(b g(c a b)) a f(a b))";

    auto termA = parse(sig, bank, inputA);
    auto termB = parse(sig, bank, inputB);

    auto instruct = check_C_eq(termA, termB, bank, {"f", "g"});
    EXPECT_TRUE(instruct.has_value());

    auto actual_res = apply_CInstruct(termA, *instruct, bank);
    auto expected_res = termB;
    EXPECT_EQ(actual_res, expected_res);
}
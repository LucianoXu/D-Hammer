
#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;


TEST(TestACflatten, flatten) {
    Signature<string> sig = {
        {{"f", "f"}, {"a", "a"}, {"b", "b"}},
    };

    auto t = sig.parse("f[a, f[a, b], b]");

    auto actual_res = flatten(t, {"f"});
    auto expected_res = sig.parse("f[a, a, b, b]");

    EXPECT_EQ(*actual_res, *expected_res);
}


//////////////////////////////////////
// AC-theory by normal terms + sorting

TEST(TestCProofInstruct, check_C_eq1) {

    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}, {"a", "a"}, {"b", "b"}, {"c", "c"}}
    };

    string inputA = "f[a, g[b, g[a, c, b]], f[a, b]]";
    string inputB = "f[g[b, g[c, a, b]], a, f[a, b]]";

    auto termA = sig.parse(inputA);
    auto termB = sig.parse(inputB);

    EXPECT_TRUE(check_C_eq(termA, termB, {"f", "g"}));
}
#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;


// Test whether the term are merged correctly
TEST(TestUALGParser, Basics) {
    TermBank bank{};

    // for variables
    auto t1 = bank.get_normal_term("t", {});
    auto t2 = parse(bank, "t");
    EXPECT_EQ(t1, t2);

    // for normal terms
    auto s = bank.get_normal_term("s", {});
    auto a = bank.get_normal_term("&", {t1, s});
    auto b = parse(bank, "&(t, s)");
    EXPECT_EQ(a, b);
}
#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;

TEST(TermParsing, Basics1) {
    TermBank<string> bank{};
    Signature<string> sig = {
        {{"f", "f"}}
    };

    auto actual_res = parse(sig, bank, "f");
    auto expected_res = bank.get_term("f");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(TermParsing, Basics2) {
    TermBank<string> bank{};
    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}}
    };

    auto actual_res = parse(sig, bank, "f(g g)");
    auto expected_res = bank.get_term("f", {bank.get_term("g"), bank.get_term("g")});

    EXPECT_EQ(actual_res, expected_res);
}

TEST(TermParsing, AutomaticRegistration) {
    TermBank<string> bank{};
    Signature<string> sig = {
        {}
    };

    auto actual_res = parse(sig, bank, "f(g g)");
    auto expected_res = bank.get_term("f", {bank.get_term("g"), bank.get_term("g")});

    EXPECT_EQ(actual_res, expected_res);
}
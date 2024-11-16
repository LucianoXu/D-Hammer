#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;

TEST(TermParsing, Basics1) {
    TermBank<string> bank{};
    Signature sig = {
        {"f", SymbolType::NORMAL}
    };

    auto actual_res = parse(sig, bank, "f");
    auto expected_res = bank.get_normal_term("f", {});

    EXPECT_EQ(actual_res, expected_res);
}

TEST(TermParsing, Basics2) {
    TermBank<string> bank{};
    Signature sig = {
        {"f", SymbolType::NORMAL},
        {"g", SymbolType::NORMAL}
    };

    auto actual_res = parse(sig, bank, "f(g g)");
    auto expected_res = bank.get_normal_term("f", {bank.get_normal_term("g", {}), bank.get_normal_term("g", {})});

    EXPECT_EQ(actual_res, expected_res);
}

TEST(TermParsing, CTerms) {
    TermBank<string> bank{};
    Signature sig = {
        {"f", SymbolType::NORMAL},
        {"g", SymbolType::NORMAL},
        {"&", SymbolType::C}
    };

    auto actual_res1 = parse(sig, bank, "&(g f(g))");
    auto actual_res2 = parse(sig, bank, "&(f(g) g)");
    auto expected_res = bank.get_c_term("&", {{bank.get_normal_term("g", {}), 1}, {bank.get_normal_term("f", {bank.get_normal_term("g", {})}), 1}});

    EXPECT_EQ(actual_res1, expected_res);
    EXPECT_EQ(actual_res2, expected_res);
}

TEST(TermParsing, ACTerms) {
    TermBank<string> bank{};
    Signature sig = {
        {"f", SymbolType::NORMAL},
        {"g", SymbolType::NORMAL},
        {"&", SymbolType::AC}
    };

    auto actual_res1 = parse(sig, bank, "&(g f(g) &(g))");
    auto actual_res2 = parse(sig, bank, "&(f(g) g g)");
    auto expected_res = bank.get_ac_term("&", {{bank.get_normal_term("g", {}), 2}, {bank.get_normal_term("f", {bank.get_normal_term("g", {})}), 1}});

    EXPECT_EQ(actual_res1, expected_res);
    EXPECT_EQ(actual_res2, expected_res);
}
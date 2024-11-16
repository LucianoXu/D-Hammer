#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;

REWRITE_DEF(string, rule1, bank, term) {
    if (term->get_head() == "f") {
        return bank.get_normal_term("g", {});
    }
    return std::nullopt;
}

REWRITE_DEF(string, rule2, bank, term) {
    if (term->get_head() == "g") {
        return bank.get_normal_term("l", {});
    }
    return std::nullopt;
}


TEST(TermRewriting, Basics1) {
    TermBank<string> bank{};
    Signature sig = {
        {"f", SymbolType::NORMAL},
        {"g", SymbolType::NORMAL}
    };
    
    // for variables
    auto input = parse(sig, bank, "f");

    auto actual_res = rewrite_all(bank, input, {rule1, rule2});
    auto expected_res = parse(sig, bank, "g");

    EXPECT_EQ(actual_res, expected_res);
}


TEST(TermRewriting, Basics2) {
    TermBank<string> bank{};
    Signature sig = {
        {"f", SymbolType::NORMAL},
        {"g", SymbolType::NORMAL},
        {"h", SymbolType::NORMAL}
    };

    // for variables
    auto input = parse(sig, bank, "h(f f f)");

    auto actual_res = rewrite_all(bank, input, {rule1, rule2});
    auto expected_res = parse(sig, bank, "h(g g g)");
    
    EXPECT_EQ(actual_res, expected_res);
}


TEST(TermRewriting, Basics3) {
    TermBank<string> bank{};
    Signature sig = {
        {"d", SymbolType::NORMAL},
        {"e", SymbolType::NORMAL},
        {"h", SymbolType::NORMAL}
    };
    
    // for variables
    auto input = parse(sig, bank, "h(d e e)");

    auto actual_res = rewrite_all(bank, input, {rule1, rule2});
    auto expected_res = std::nullopt;
    
    EXPECT_EQ(actual_res, expected_res);
}


TEST(TermRewriting, rewrite_repeated) {
    TermBank<string> bank{};
    Signature sig = {
        {"f", SymbolType::NORMAL},
        {"h", SymbolType::NORMAL},
        {"l", SymbolType::NORMAL}
    };

    // for variables
    auto input = parse(sig, bank, "h(f f f)");

    auto actual_res = rewrite_repeated(bank, input, {rule1, rule2});
    auto expected_res = parse(sig, bank, "h(l l l)");
    
    EXPECT_EQ(actual_res, expected_res);
}
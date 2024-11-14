#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;

REWRITE_DEF(rule1, bank, term) {
    if (term->get_head() == "f") {
        return bank.get_normal_term("g", {});
    }
    return std::nullopt;
}

REWRITE_DEF(rule2, bank, term) {
    if (term->get_head() == "g") {
        return bank.get_normal_term("l", {});
    }
    return std::nullopt;
}


TEST(TermRewriting, Basics1) {
    TermBank bank{};
    
    // for variables
    auto input = parse(bank, "f");

    auto actual_res = rewrite_all(bank, input, {rule1, rule2});
    auto expected_res = parse(bank, "g");

    EXPECT_EQ(actual_res, expected_res);
}


TEST(TermRewriting, Basics2) {
    TermBank bank{};
    
    // for variables
    auto input = parse(bank, "h(f, f, f)");

    auto actual_res = rewrite_all(bank, input, {rule1, rule2});
    auto expected_res = parse(bank, "h(g, g, g)");
    
    EXPECT_EQ(actual_res, expected_res);
}


TEST(TermRewriting, Basics3) {
    TermBank bank{};
    
    // for variables
    auto input = parse(bank, "h(d, e, e)");

    auto actual_res = rewrite_all(bank, input, {rule1, rule2});
    auto expected_res = std::nullopt;
    
    EXPECT_EQ(actual_res, expected_res);
}


TEST(TermRewriting, rewrite_repeated) {
    TermBank bank{};
    
    // for variables
    auto input = parse(bank, "h(f, f, f)");

    auto actual_res = rewrite_repeated(bank, input, {rule1, rule2});
    auto expected_res = parse(bank, "h(l, l, l)");
    
    EXPECT_EQ(actual_res, expected_res);
}
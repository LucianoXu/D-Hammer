#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;

TEST(TermParsing, Basics1) {
    Signature<string> sig = {
        {{"f", "f"}}
    };

    auto actual_res = sig.parse("f");
    auto expected_res = make_shared<const Term<string>>("f");

    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(TermParsing, Basics2) {
    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}}
    };

    auto actual_res = sig.parse("f[g, g]");
    auto expected_res = make_shared<const Term<string>>("f", vector{make_shared<const Term<string>>("g"), make_shared<const Term<string>>("g")});

    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(TermParsing, AutomaticRegistration) {
    Signature<string> sig = {
        {}
    };

    auto actual_res = sig.parse("f[g, g]");
    auto expected_res = make_shared<const Term<string>>("f", vector{make_shared<const Term<string>>("g"), make_shared<const Term<string>>("g")});

    EXPECT_EQ(*actual_res, *expected_res);
}

TEST(TermParsing, Term2Ast) {
    Signature<string> sig = {
        {{"f", "f"}, {"g", "g"}}
    };

    auto ast = astparser::AST{
        "f",
        vector{astparser::AST{"g"}, astparser::AST{"g"}}
    };

    auto term = sig.ast2term(ast);

    auto actual_res = sig.term2ast(term);

    EXPECT_EQ(actual_res, ast);
}
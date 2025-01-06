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
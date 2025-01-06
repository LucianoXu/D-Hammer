#include <gtest/gtest.h>

#include "ualg.hpp"

using namespace ualg;
using namespace std;

///////////////////
// NormalTerm
TEST(TestTerm, get_term_size) {

    auto t = make_shared<const Term<string>>("t", vector<TermPtr<string>>{});
    auto s = make_shared<const Term<string>>("s", vector<TermPtr<string>>{t});
    auto r = make_shared<const Term<string>>("r", vector<TermPtr<string>>{t});
    auto a = make_shared<const Term<string>>("&", vector<TermPtr<string>>{s, r});

    EXPECT_EQ(a->get_term_size(), 5);
}

TEST(TestTerm, get_subterm) {

    auto t = make_shared<const Term<string>>("t", vector<TermPtr<string>>{});
    auto s = make_shared<const Term<string>>("s", vector<TermPtr<string>>{t});
    auto r = make_shared<const Term<string>>("r", vector<TermPtr<string>>{t});
    auto a = make_shared<const Term<string>>("&", vector<TermPtr<string>>{s, r});

    auto subterm = a->get_subterm({0});
    EXPECT_EQ(subterm, s);
}

TEST(TestTerm, replace_at) {

    auto s = make_shared<const Term<string>>("s", vector<TermPtr<string>>{});
    auto t = make_shared<const Term<string>>("t", vector<TermPtr<string>>{});
    auto a = make_shared<const Term<string>>("&", vector<TermPtr<string>>{s, s});

    // replacement
    auto actual_res = a->replace_at({0}, t);

    auto expected_res = make_shared<const Term<string>>("&", vector<TermPtr<string>>{t, s});

    EXPECT_EQ(*actual_res, *expected_res);
}
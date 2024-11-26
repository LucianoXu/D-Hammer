#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;


TEST(DiracoqReduction, Beta) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("T"));

    auto term = kernel.parse("apply(fun(x T x) a)");
    auto normal_term = static_cast<const NormalTerm<int>*>(term);

    auto actual_res = pos_rewrite_repeated(kernel, normal_term, rules);
    auto expected_res = kernel.parse("a");

    EXPECT_EQ(actual_res, expected_res);
}


TEST(DiracoqReduction, default_searcher) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("a"), kernel.parse("T"));
    kernel.def(kernel.register_symbol("f"), kernel.parse("fun(x T x)"));

    auto term = kernel.parse("apply(f a)");
    auto normal_term = static_cast<const NormalTerm<int>*>(term);

    auto actual_res = pos_rewrite_repeated(kernel, normal_term, rules);
    auto expected_res = kernel.parse("a");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqReduction, Eta) {
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("Arrow(T T)"));

    auto term = kernel.parse("fun(x T apply(f x))");
    auto normal_term = static_cast<const NormalTerm<int>*>(term);

    auto actual_res = pos_rewrite_repeated(kernel, normal_term, rules);
    auto expected_res = kernel.parse("f");

    cout << kernel.term_to_string(actual_res) << endl;
    cout << kernel.term_to_string(expected_res) << endl;

    EXPECT_EQ(actual_res, expected_res);
}
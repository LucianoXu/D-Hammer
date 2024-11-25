#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqParsing, Basics1) {
    
    Kernel kernel;

    auto& bank = kernel.get_bank();
    auto& sig = kernel.get_sig();

    auto actual_res = kernel.parse("forall(x y apply(z x))");
    
    auto expected_res = bank.get_normal_term(
        sig.get_repr("forall"), {
            bank.get_normal_term(sig.get_repr("x"), {}),
            bank.get_normal_term(sig.get_repr("y"), {}),
            bank.get_normal_term(sig.get_repr("apply"), {
                bank.get_normal_term(sig.get_repr("z"), {}),
                bank.get_normal_term(sig.get_repr("x"), {})
            })
        }
    );

    EXPECT_EQ(actual_res, expected_res);
}


TEST(DiracoqParsing, Basics2) {
    
    Kernel kernel;

    EXPECT_EQ(kernel.term_to_string(kernel.parse("Type")), "Type");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("Type(Type)")), "Type(Type)");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("forall(x y apply(z x))")), "forall(x y apply(z x))");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("fun(x y apply(z x))")), "fun(x y apply(z x))");

}

TEST(DiracoqTypeCalc, successes) {
    Kernel kernel;

    // (Assum)
    kernel.assum(kernel.register_symbol("x"), kernel.parse("Type"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("x")), kernel.parse("Type"));
    kernel.env_pop();

    // (Def)
    kernel.def(kernel.register_symbol("f"), kernel.parse("fun (x Type x)"), kernel.parse("Arrow(Type Type)"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("f")), kernel.parse("Arrow(Type Type)"));

    // (App)
    kernel.assum(kernel.register_symbol("x"), kernel.parse("Type"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("apply(f x)")), kernel.parse("Type"));

}

TEST(DiracoqTypeCalc, errors) {
    Kernel kernel;

    EXPECT_THROW(kernel.env_pop(), std::runtime_error);

    EXPECT_THROW(kernel.calc_type(kernel.parse("Type")), std::runtime_error);
}
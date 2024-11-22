#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqParsing, Basics1) {
    
    Kernel kernel(CoC_sig);

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
    
    Kernel kernel(CoC_sig);

    EXPECT_EQ(kernel.term_to_string(kernel.parse("Type")), "Type");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("Type(Type)")), "Type(Type)");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("forall(x y apply(z x))")), "forall(x y apply(z x))");
    EXPECT_EQ(kernel.term_to_string(kernel.parse("fun(x y apply(z x))")), "fun(x y apply(z x))");

}

TEST(DiracoqTypeCalc, successes) {
    Kernel kernel(CoC_sig);


    // (Ax-Type)
    EXPECT_EQ(kernel.calc_type(kernel.parse("Type")), kernel.parse("Type(Type)"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("Type(Type)")), kernel.parse("Type(Type(Type))"));

    // (Var)
    kernel.local_assum(kernel.register_symbol("x"), kernel.parse("Type"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("x")), kernel.parse("Type"));
    kernel.local_pop();

    // (Const)
    kernel.local_def(kernel.register_symbol("f"), kernel.parse("fun (x Type x)"), kernel.parse("forall(x Type Type)"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("f")), kernel.parse("forall(x Type Type)"));

    // (App)
    kernel.local_assum(kernel.register_symbol("x"), kernel.parse("Type"));
    EXPECT_EQ(kernel.calc_type(kernel.parse("apply(f x)")), kernel.parse("Type"));

}

TEST(DiracoqTypeCalc, errors) {
    Kernel kernel(CoC_sig);

    EXPECT_THROW(kernel.local_pop(), std::runtime_error);

    EXPECT_THROW(kernel.calc_type(kernel.parse("Type(x)")), std::runtime_error);

    EXPECT_THROW(kernel.calc_type(kernel.parse("Type(x)")), std::runtime_error);
}
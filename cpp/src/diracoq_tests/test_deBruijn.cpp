#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(Dirqcoq_deBruijn, Instantiate1) {
    
    Kernel kernel;

    auto expr = kernel.parse("fun(fun($2))");

    auto actual_res = instantiate(kernel.get_bank(), 
        static_cast<const NormalTerm<int>*>(expr), 
        static_cast<const NormalTerm<int>*>(kernel.parse("x"))
    );

    auto expected_res = kernel.parse("fun(fun(x))");

    EXPECT_EQ(actual_res, expected_res);
}


TEST(Dirqcoq_deBruijn, Instantiate2) {
    
    Kernel kernel;

    auto expr = kernel.parse("fun(fun($2))");

    auto actual_res = instantiate(kernel.get_bank(), 
        static_cast<const NormalTerm<int>*>(expr), 
        static_cast<const NormalTerm<int>*>(kernel.parse("$0"))
    );

    auto expected_res = kernel.parse("fun(fun($2))");

    EXPECT_EQ(actual_res, expected_res);
}
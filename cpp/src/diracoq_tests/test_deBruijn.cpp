#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(Diraqcoq_deBruijn, Instantiate1) {
    
    Kernel kernel;

    auto expr = kernel.parse("fun(fun($2))");

    auto actual_res = instantiate(kernel.get_bank(), expr, 0, kernel.parse("x"));

    auto expected_res = kernel.parse("fun(fun(x))");

    EXPECT_EQ(actual_res, expected_res);
}


TEST(Diraqcoq_deBruijn, Instantiate2) {
    
    Kernel kernel;

    auto expr = kernel.parse("fun(fun($2))");

    auto actual_res = instantiate(kernel.get_bank(), expr, 0, kernel.parse("$1"));

    auto expected_res = kernel.parse("fun(fun($2))");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(Diraqcoq_deBruijn, Instantiate3) {
    
    Kernel kernel;

    auto expr = kernel.parse("fun(fun(apply($2 apply($3 $4))))");

    auto actual_res = instantiate(kernel.get_bank(), expr, 1, kernel.parse("x"));

    auto expected_res = kernel.parse("fun(fun(apply($2 apply(x $3))))");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(Diraqcoq_deBruijn, Instantiate4) {
    
    Kernel kernel;

    auto expr = kernel.parse("fun(fun(apply($2 apply($3 $4))))");

    auto actual_res = instantiate(kernel.get_bank(), expr, 1, kernel.parse("$5"));

    auto expected_res = kernel.parse("fun(fun(apply($2 apply($6 $3))))");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(Diraqcoq_deBruijn, Instantiate5) {
    
    Kernel kernel;

    auto expr = kernel.parse("fun(fun(apply($2 apply($3 $4))))");

    auto actual_res = instantiate(kernel.get_bank(), expr, 1, kernel.parse("apply($5 fun($6))"));

    auto expected_res = kernel.parse("fun(fun(apply($2 apply(apply($6 fun($7)) $3))))");

    EXPECT_EQ(actual_res, expected_res);
}

TEST(Diraqcoq_deBruijn, swap1) {

    Kernel kernel;

    auto expr = kernel.parse("apply($0 $1)");

    auto actual_res = deBruijn_swap(kernel.get_bank(), expr, 0, 1);

    auto expected_res = kernel.parse("apply($1 $0)");

    EXPECT_EQ(actual_res, expected_res);
}


TEST(Diraqcoq_deBruijn, swap2) {

    Kernel kernel;

    auto expr = kernel.parse("apply($0 fun($1 $0))");

    auto actual_res = deBruijn_swap(kernel.get_bank(), expr, 0, 1);

    cout << kernel.term_to_string(actual_res) << endl;

    auto expected_res = kernel.parse("apply($1 fun($0 $0))");

    EXPECT_EQ(actual_res, expected_res);
}
#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;


TEST(DiracoqTrace2Coq, scalars) {
    Kernel kernel;
    auto term = kernel.parse("MULS(a ADDS(b c) b 1 ADDS(a b 0))");
    auto actual_res = term_to_coq(kernel, term);
    auto expected_res = "[* a; [+ b; c]; b; 1; [+ a; b; 0]]";
    EXPECT_EQ(actual_res, expected_res);
}

TEST(DiracoqTrace2Coq, NormalTermPos) {
    NormalTermPos pos = {1, 2, 3};
    auto actual_res = pos_to_string(pos);
    auto expected_res = "(P_ac 1 (P_ac 2 (P_ac 3 P_all)))";
    EXPECT_EQ(actual_res, expected_res);
}
#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqSpeicalEq, Delta) {
    Prover prover;
    prover.process("Group(Var(T Base) Var(a T) Var(b T))");
    EXPECT_TRUE(prover.check_eq("DELTA(a b)", "DELTA(b a)"));
}


TEST(DiracoqSpecialEq, Alpha) {
    Prover prover;
    prover.process("Group(Var(T Base) Var(a T) Var(b T))");
    EXPECT_TRUE(prover.check_eq("fun(x T a)", "fun(y T a)"));
}
#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;


TEST(DiracoqSpeicalEq, Dirac) {
    Prover prover;
    prover.process("Group(Var(T Base) Var(a T) Var(b T))");
    EXPECT_TRUE(prover.check_eq("DELTA(a b)", "DELTA(b a)"));
}
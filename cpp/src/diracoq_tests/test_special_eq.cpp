#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqSpeicalEq, Delta) {
    Prover prover;
    prover.process("Var T : INDEX. Var a : BASIS(T). Var b : BASIS(T).");
    EXPECT_TRUE(prover.check_eq("DELTA(a, b)", "DELTA(b, a)"));
}


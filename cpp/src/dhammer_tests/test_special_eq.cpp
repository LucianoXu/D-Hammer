#include <gtest/gtest.h>

#include "dhammer.hpp"

using namespace ualg;
using namespace std;
using namespace dhammer;

TEST(dhammerSpecialEq, Delta) {
    Prover prover;
    prover.process("Var T : INDEX. Var a : BASIS[T]. Var b : BASIS[T].");
    EXPECT_TRUE(prover.check_eq("DELTA[a, b]", "DELTA[b, a]"));
}

TEST(dhammerSpecialEq, SUM_SWAP) {
    Prover prover;

    prover.process(
        R"(
            Var T : INDEX.
            Var M : INDEX.
        )");

    EXPECT_TRUE(prover.check_eq(
        "Sum i in USET[T], Sum j in USET[M], 1",
        "Sum j in USET[M], Sum i in USET[T], 1"
    ));
}

TEST(dhammerSpecialEq, RSET) {
    Kernel kernel;

    EXPECT_TRUE(is_eq_modulo_rset(kernel.parse("RSET[1, 2, 3]"), kernel.parse("RSET[3, 2, 1]")));
}
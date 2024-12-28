#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqProver, ShowAll) { 
    Prover prover;
    EXPECT_TRUE(prover.process("SHOWALL."));
}

TEST(DiracoqProver, Var) {
    Prover prover;
    EXPECT_TRUE(prover.process("Var x : TYPE."));
    EXPECT_FALSE(prover.process("Var x : TYPE."));
}

TEST(DiracoqProver, Normalize) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : STYPE.
        Var b : STYPE.
        Var c : STYPE.
        Normalize a * (b + c) * b * 1 * (a + b + 0).
        )")
    );
}


TEST(DiracoqProver, CheckEq1) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : STYPE. 
        Var b : STYPE. 
        Var c : STYPE.
        CheckEq a * (b + c) * b * 1 * (a + b + 0) with (b + c) * (a * b * 1) * (a + b).
        )")
    );
}

TEST(DiracoqProver, CheckEq2) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : STYPE. 
        Var b : STYPE. 
        Var T : INDEX. 
        Var K : KTYPE(T).
        CheckEq a b K with (a*b).K.
        )")
    );
}
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
        NORMALIZE MULS(a ADDS(b c) b 1 ADDS(a b 0)).
        )")
    );
}


TEST(DiracoqProver, CheckEq1) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : STYPE. 
        Var b : STYPE. 
        Var c : STYPE.
        CHECK MULS(a ADDS(b c) b 1 ADDS(a b 0)) = MULS(ADDS(b c) MULS(a b 1) ADDS(a b)).
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
        CHECK COMPO(COMPO(a b) K) = SCR(MULS(a b) K).
        )")
    );
}
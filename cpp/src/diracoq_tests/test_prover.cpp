#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqProver, ShowAll) { 
    Prover prover;
    EXPECT_TRUE(prover.process("ShowAll."));
}

TEST(DiracoqProver, Var) {
    Prover prover;
    EXPECT_TRUE(prover.process("Var x : Type."));
    EXPECT_FALSE(prover.process("Var x : Type."));
}

TEST(DiracoqProver, Normalize) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : SType.
        Var b : SType.
        Var c : SType.
        Normalize MULS(a ADDS(b c) b 1 ADDS(a b 0)).
        )")
    );
}


TEST(DiracoqProver, CheckEq1) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : SType. 
        Var b : SType. 
        Var c : SType.
        Check MULS(a ADDS(b c) b 1 ADDS(a b 0)) = MULS(ADDS(b c) MULS(a b 1) ADDS(a b)).
        )")
    );
}

TEST(DiracoqProver, CheckEq2) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : SType. 
        Var b : SType. 
        Var T : Index. 
        Var K : KType(T).
        Check COMPO(COMPO(a b) K) = SCR(MULS(a b) K).
        )")
    );
}
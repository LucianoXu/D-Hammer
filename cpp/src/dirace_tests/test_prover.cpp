#include <gtest/gtest.h>

#include "dirace.hpp"

using namespace ualg;
using namespace std;
using namespace dirace;

TEST(DiraceProver, ShowAll) { 
    Prover prover;
    EXPECT_TRUE(prover.process("SHOWALL."));
}

TEST(DiraceProver, Var) {
    Prover prover;
    EXPECT_TRUE(prover.process("Var x : TYPE."));
    EXPECT_FALSE(prover.process("Var x : TYPE."));
}

TEST(DiraceProver, Normalize) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : STYPE.
        Var b : STYPE.
        Var c : STYPE.
        Normalize a * (b + c) * b * 1 * (a + b + 0).
        )")
    );
}


TEST(DiraceProver, CheckEq1) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : STYPE. 
        Var b : STYPE. 
        Var c : STYPE.
        CheckEq a * (b + c) * b * 1 * (a + b + 0) with (b + c) * (a * b * 1) * (a + b).
        )")
    );
}

TEST(DiraceProver, CheckEq2) {
    Prover prover;
    EXPECT_TRUE(prover.process(R"(
        Var a : STYPE. 
        Var b : STYPE. 
        Var T : INDEX. 
        Var K : KTYPE[T].
        CheckEq a b K with (a*b).K.
        )")
    );
}
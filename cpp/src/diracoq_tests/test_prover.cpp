#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqProver, ShowAll) { 
    Prover prover;
    EXPECT_TRUE(prover.process("ShowAll"));
}

TEST(DiracoqProver, Var) {
    Prover prover;
    EXPECT_TRUE(prover.process("Var(x Type)"));
    EXPECT_FALSE(prover.process("Var(x Type)"));
}

TEST(DiracoqProver, Normalize) {
    Prover prover;
    EXPECT_TRUE(prover.process("\
        Group(\n\
            Var(a SType) Var(b SType) Var(c SType)\n\
            Normalize(MULS(a ADDS(b c) b 1 ADDS(a b 0)))\n\
        )")
    );
}


TEST(DiracoqProver, CheckEq) {
    Prover prover;
    EXPECT_TRUE(prover.process("\
        Group(\n\
            Var(a SType) Var(b SType) Var(c SType)\n\
            CheckEq(MULS(a ADDS(b c) b 1 ADDS(a b 0)) MULS(ADDS(b c) MULS(a b 1) ADDS(a b)))\n\
        )")
    );
}
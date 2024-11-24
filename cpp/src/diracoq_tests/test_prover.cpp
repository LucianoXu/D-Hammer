#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqProver, Basics1) { 
    Prover prover;
    prover.process("ShowAll");
}
    
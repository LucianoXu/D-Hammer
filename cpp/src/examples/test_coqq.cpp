
#include <gtest/gtest.h>

#include "diracoq.hpp"
#include "examples.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;
using namespace examples;

class EqExampleTest : public ::testing::TestWithParam<EqExample> {
protected:
    void RunTest(const EqExample& example) {
        auto prover = std_prover();
        prover->process(example.preproc_code);
        cout << "TEST NAME: " << example.name << endl;
        EXPECT_EQ(prover->check_eq(example.termA, example.termB), example.expected_res);
        delete prover;
    }
};

TEST_P(EqExampleTest, CheckEquality) {
    // Get the current test parameter
    EqExample example = GetParam();

    // Run the test logic
    RunTest(example);
}

// Instantiate the test suite with the example list
INSTANTIATE_TEST_SUITE_P(
    EqExampleTests,  // Test suite name
    EqExampleTest,   // Test case class
    ::testing::ValuesIn(eq_examples)  // Provide the vector of examples
);
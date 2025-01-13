
#include <gtest/gtest.h>

#include "dirace.hpp"
#include "examples.hpp"

using namespace ualg;
using namespace std;
using namespace dirace;
using namespace examples;


unique_ptr<Prover> prover = nullptr;

unique_ptr<Prover> init_prover() {
    if (!prover) {

        // use the Wolfram Engine on MacOS
        auto [ep, lp] = wstp::init_and_openlink(wstp::MACOS_ARGC, wstp::MACOS_ARGV);

        cout << "WSTP link: " << lp << endl;
        
        prover = make_unique<Prover>(std_prover(lp));

        // avoid using the Wolfram Engine
        // prover = make_unique<Prover>(std_prover());
    }
    return make_unique<Prover>(*prover);
}

class EqExampleTest : public ::testing::TestWithParam<EqExample> {
protected:
    void RunTest(const EqExample& example) {
        cout << "TEST NAME: " << example.name << endl;
        auto new_prover = init_prover();
        new_prover->process(example.preproc_code);
        EXPECT_EQ(new_prover->check_eq(example.termA, example.termB), example.expected_res);
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

INSTANTIATE_TEST_SUITE_P(
    LabelledEqExampleTests,  // Test suite name
    EqExampleTest,   // Test case class
    ::testing::ValuesIn(labelled_eq_examples)  // Provide the vector of examples
);
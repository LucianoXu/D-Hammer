
#include <gtest/gtest.h>

#include "dirace.hpp"
#include "examples.hpp"

using namespace ualg;
using namespace std;
using namespace dirace;
using namespace examples;

#include <chrono>

void test_examples(const vector<EqExample>& examples) {
    
    // use the Wolfram Engine on MacOS
    auto [ep, lp] = wstp::init_and_openlink(wstp::MACOS_ARGC, wstp::MACOS_ARGV);
    
    auto prover = make_unique<Prover>(std_prover(lp));
    prover->process("Normalize Times[I, I].");

    auto start = std::chrono::high_resolution_clock::now();

    for (auto example : examples) {
        prover = make_unique<Prover>(std_prover(lp));
        prover->process(example.preproc_code);
        prover->check_eq(example.termA, example.termB);
    }

    auto end = std::chrono::high_resolution_clock::now();

    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end - start);

    std::cout << "Example Number: " << examples.size() << std::endl;
    std::cout << "DURATION: " << duration.count() << " ms" << std::endl;
}

TEST(TestTiming, QCQI) {
    test_examples(QCQI_examples);
}

TEST(TestTiming, CoqQ) {
    test_examples(CoqQ_examples);
}

TEST(TestTiming, Circuit) {
    test_examples(Circuit_examples);
}

TEST(TestTiming, Jens2024) {
    test_examples(Jens2024_examples);
}
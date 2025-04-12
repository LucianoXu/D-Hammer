

#include "ualg.hpp"
#include "dhammer.hpp"
#include "examples.hpp"

using namespace ualg;
using namespace std;
using namespace dhammer;

int main(int argc, const char ** argv) {

    cout << "< D-Hammer Prover top level built by Yingte Xu." << endl;

    // use the Wolfram Engine with appropriate arguments
    cout << "Arguments: ";
    for (int i = 0; i < argc; i++) {
        cout << argv[i] << " ";
    }
    cout << endl;
    auto [ep, lp] = (argc > 1)
        ? wstp::init_and_openlink(argc, const_cast<char**>(argv))
        : wstp::init_and_openlink(wstp::MACOS_ARGC, wstp::MACOS_ARGV);
    cout << "WSTP link: " << lp << endl;

    auto prover = make_unique<Prover>(std_prover(lp));

    // put the code for debugging here
    prover->process(
        R"(
            Def plus := Divide[1, Sqrt[2]] . (|#0> + |#1>).
            Def minus := Divide[1, Sqrt[2]] . (|#0> + (-1).|#1>).
            Def X := |#0><#1| + |#1><#0|.
            Var q : REG[BIT]. Var q1 : REG[BIT]. Var q2 : REG[BIT].
            Normalize (-1) . |#0>_q (plus minus)_(q1, q2).
            Normalize X_q2 |#0>_q (plus minus)_(q1, q2).
        )");

    return 0;
}
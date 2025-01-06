

#include "ualg.hpp"
#include "diracoq.hpp"


using namespace ualg;
using namespace std;
using namespace diracoq;

int main(int , const char **) {
    
    Prover* prover = std_prover();

    cout << "Diracoq Prover top level built by Yingte Xu." << endl;

    // prover->process(R"(
    //             Var T1 : INDEX.
    //             Var T2 : INDEX.
    //             Var T3 : INDEX.
    //             Var T4 : INDEX.
    //             Var A : OTYPE[T1 * T2, T3 * T4].
    // )");

    // prover->process("Normalize A with trace.");
    // // prover->process("Normalize Tr (T1 * T2) (A * B) with trace.");
    
    // return 0;

    while (true) {
        string code;
        cout << "> ";
        getline(cin, code);

        prover->process(code);
    }

    delete prover;

    return 0;
}

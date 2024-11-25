

#include "ualg.hpp"
#include "scalar.hpp"
#include "scalar_vec.hpp"
#include "diracoq.hpp"


using namespace ualg;
using namespace std;
using namespace scalar_vec;
using namespace diracoq;

int main(int , const char **) {

    Prover prover;

    cout << "Diracoq Prover top level built by Yingte Xu." << endl;

    while (true) {
        string code;
        cout << "> ";
        getline(cin, code);

        if (code == "exit") {
            break;
        }
        prover.process(code);
    }

    return 0;
}

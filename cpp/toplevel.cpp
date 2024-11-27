

#include "ualg.hpp"
#include "scalar.hpp"
#include "scalar_vec.hpp"
#include "diracoq.hpp"


using namespace ualg;
using namespace std;
using namespace scalar_vec;
using namespace diracoq;

#include <csignal>

void signalHandler(int signum) {
    std::cout << "Interrupt signal (" << signum << ") received.\n";
    // Add cleanup logic here (e.g., close files, release resources, etc.)
    exit(signum);
}

int main(int argc, const char **argv) {
    // Register signal handler
    signal(SIGINT, signalHandler);


    Prover* prover;

    if (argc == 1) {
        prover = new Prover{std::cout, false};
    }

    else if (argc == 2) {
        prover = new Prover{std::cout, true, argv[1]};
    }

    else {
        cout << "Too many arguments." << endl;
        return 1;
    }

    cout << "Diracoq Prover top level built by Yingte Xu." << endl;

    while (true) {
        string code;
        cout << "> ";
        getline(cin, code);

        prover->process(code);
    }

    delete prover;

    return 0;
}

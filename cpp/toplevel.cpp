

#include "ualg.hpp"
#include "diracoq.hpp"


using namespace ualg;
using namespace std;
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
        prover = new Prover{std::cout};
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

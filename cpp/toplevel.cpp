
#include "ualg.hpp"
#include "dirace.hpp"


using namespace ualg;
using namespace std;
using namespace dirace;

#include <csignal>

void signalHandler(int signum) {
    std::cout << std::endl << "< Interrupt signal (" << signum << ") received.\n";
    // Add cleanup logic here (e.g., close files, release resources, etc.)
    exit(signum);
}

int main(int argc, const char **argv) {

    // Register signal handler
    signal(SIGINT, signalHandler);

    cout << "< Dirace Prover top level built by Yingte Xu." << endl;

    auto [formatted_argc, formatted_argv] = wstp::args_format(argc, argv);

    auto [ep, lp] = wstp::init_and_openlink(formatted_argc, formatted_argv);

    if (!lp) {
        cout << "< Failed to establish WSTP link. The prover will run without Wolfram Engine." << endl;
    }
    else {
        cout << "< WSTP link established." << endl;
    }

    Prover prover = std_prover(lp);

    while (true) {
        string code;
        cout << "> ";
        getline(cin, code);

        prover.process(code);
    }

    return 0;
}

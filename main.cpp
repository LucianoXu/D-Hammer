

#include <iostream>
#include <map>
#include <string>

int main(int , const char **) {

    return 0;
}

// #include "ualg.hpp"
// #include "scalar.hpp"


// using namespace ualg;
// using namespace std;

// int main(int , const char **) {
//     TermBank bank{};

//     auto zero = bank.get_normal_term("0", {});

//     auto s = bank.get_normal_term("s", {});
//     auto t = bank.get_normal_term("t", {});

//     auto adds = bank.get_ac_term("ADDS", {{zero, 1}, {t, 1}, {s, 1}});

//     auto res = rewrite_repeated(bank, adds, {scalar::R_ADD0});

//     cout << res->to_string() << endl;

//     return 0;
// }

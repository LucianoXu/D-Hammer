

#include <iostream>
#include <map>
#include <string>

#include "ualg.hpp"


using namespace ualg;
using namespace std;

int main(int , const char **) {
    TermBank bank{};

    auto s = bank.get_ac_term("s", {});
    auto t = bank.get_ac_term("t", {});
    auto a = bank.get_ac_term("&", {{s, 1}, {t, 1}});
    auto actual_res = bank.get_ac_term("&", {{a, 1}, {t, 1}});
    auto expected_res = bank.get_ac_term("&", {{s, 1}, {t, 2}});

    cout << actual_res->to_string() << endl;
    cout << expected_res->to_string() << endl;
}

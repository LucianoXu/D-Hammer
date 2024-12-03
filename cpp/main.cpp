

#include "ualg.hpp"
#include "diracoq.hpp"


using namespace ualg;
using namespace std;
using namespace diracoq;

void TEST_RULE(Kernel& kernel, const vector<PosRewritingRule>& rules, string input, string expected) {
    auto term = static_cast<const NormalTerm<int>*>(kernel.parse(input));
    auto actual_res = pos_rewrite_repeated(kernel, term, rules);
    auto expected_res = static_cast<const NormalTerm<int>*>(kernel.parse(expected));

    cout << "Actual: "<< kernel.term_to_string(actual_res) << endl;
    cout << "Expected: "<< kernel.term_to_string(expected_res) << endl;
}


void TEST_RULE(const vector<PosRewritingRule>& rules, string input, string expected) {
    Kernel kernel;
    TEST_RULE(kernel, rules, input, expected);
}

int main(int , const char **) {

    return 0;
}

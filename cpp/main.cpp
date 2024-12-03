

#include "ualg.hpp"
#include "scalar.hpp"
#include "scalar_vec.hpp"
#include "diracoq.hpp"


using namespace ualg;
using namespace std;
using namespace scalar_vec;
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

void demo1() {

    ////////////////////////////////////////////////////
    // speicify the variables
    vector<string> variables = {"a", "b", "c"};

    ////////////////////////////////////////////////////
    // specify the term
    string inputA = "MULS(a ADDS(b c) b 1 ADDS(a b 0))";

    TermBank<int> bank{};
    Signature<int> sig = reserved_sig;

    for (const auto& var : variables) {
        sig.register_symbol(var);
    }

    auto term = parse(sig, bank, inputA);

    cout << "Initial term:\n" << sig.term_to_string(term) << endl << endl;

    RewritingTrace<int> trace;
    auto res_term = normalize(bank, term, &trace);
    cout << "Normalized term:\n" << sig.term_to_string(res_term) << endl << endl;

    auto cmd = trace_to_string(sig, trace, scalar_printer);
    cout << "Trace:\n" << cmd << endl;

}

int main(int , const char **) {
    
    return 0;
}

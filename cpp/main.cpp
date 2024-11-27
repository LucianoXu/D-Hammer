

#include "ualg.hpp"
#include "scalar.hpp"
#include "scalar_vec.hpp"
#include "diracoq.hpp"


using namespace ualg;
using namespace std;
using namespace scalar_vec;
using namespace diracoq;

void TEST_RULE(const vector<RewritingRule<int>>& rules, vector<string> variables, string input, string expected) {
    TermBank<int> bank{};
    Signature<int> sig = reserved_sig;

    for (const auto& var : variables) {
        sig.register_symbol(var);
    }
    
    auto term = parse(sig, bank, input);
    auto actual_res = rewrite_repeated(bank, term, rules);
    auto expected_res = parse(sig, bank, expected);

    cout << "Actual: "<< sig.term_to_string(actual_res) << endl;
    cout << "Expected: "<< sig.term_to_string(expected_res) << endl;
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
    Kernel kernel;

    kernel.assum(kernel.register_symbol("T"), kernel.parse("Type"));
    kernel.assum(kernel.register_symbol("f"), kernel.parse("Arrow(T T)"));

    auto term = kernel.parse("fun(x T apply(f x))");
    auto normal_term = static_cast<const NormalTerm<int>*>(term);

    auto actual_res = pos_rewrite_repeated(kernel, normal_term, rules);
    auto expected_res = kernel.parse("f");

    cout << kernel.term_to_string(actual_res) << endl;
    cout << kernel.term_to_string(expected_res) << endl;
    
    return 0;
}

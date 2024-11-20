

#include "ualg.hpp"
#include "scalar.hpp"
#include "scalar_vec.hpp"


using namespace ualg;
using namespace std;
using namespace scalar_vec;

void TEST_RULE(const vector<RewritingRule<int>>& rules, vector<string> variables, string input, string expected) {
    TermBank<int> bank{};
    StringSymbolType variable_symbols;
    for (const auto& var : variables) {
        variable_symbols.push_back({var, SymbolType::NORMAL});
    }

    Signature<int> sig = compile_string_sig(
        extend_string_symbol_list(
        symbols,
        variable_symbols)
    );
    auto term = parse(sig, bank, input);
    auto actual_res = rewrite_repeated(bank, term, rules);
    auto expected_res = parse(sig, bank, expected);

    cout << "Actual: "<< sig.term_to_string(actual_res) << endl;
    cout << "Expected: "<< sig.term_to_string(expected_res) << endl;
}

int main(int , const char **) {

    ////////////////////////////////////////////////////
    // speicify the variables
    vector<string> variables = {"a", "b", "c"};

    ////////////////////////////////////////////////////
    // specify the term
    string inputA = "MLTS(a ADDS(b c) b 1 ADDS(a b 0))";

    TermBank<int> bank{};
    StringSymbolType variable_symbols;
    for (const auto& var : variables) {
        variable_symbols.push_back({var, SymbolType::NORMAL});
    }

    Signature<int> sig = compile_string_sig(
        extend_string_symbol_list(
        symbols,
        variable_symbols)
    );
    auto term = parse(sig, bank, inputA);

    cout << "Initial term:\n" << sig.term_to_string(term) << endl << endl;

    RewritingTrace<int> trace;
    auto res_term = normalize(bank, term, &trace);
    cout << "Normalized term:\n" << sig.term_to_string(res_term) << endl << endl;

    auto cmd = trace_to_string(sig, trace, scalar_printer);
    cout << "Trace:\n" << cmd << endl;

    return 0;
}

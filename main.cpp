

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

    TEST_RULE({R_MLTS2}, {"a", "b", "c"}, "MLTS(a ADDS(b c))", "ADDS(MLTS(a b) MLTS(a c))");
    TEST_RULE({R_MLTS2}, {"a", "b", "c"}, "MLTS(a ADDS(b c) b)", "ADDS(MLTS(a b b) MLTS(a b c))");

    return 0;
}

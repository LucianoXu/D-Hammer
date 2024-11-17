

#include "ualg.hpp"
#include "scalar.hpp"


using namespace ualg;
using namespace std;
using namespace scalar;

template <class T>
void SINGLE_RULE_TEST(RewritingRule<T> rule, vector<string> variables, string input, string expected) {
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
    auto actual_res = rewrite_repeated(bank, term, {rule});
    auto expected_res = parse(sig, bank, expected);
    
    cout << "Actual: "<< sig.term_to_string(actual_res) << endl;
    cout << "Expected: "<< sig.term_to_string(expected_res) << endl;
}

int main(int , const char **) {
    
    SINGLE_RULE_TEST(R_MLTS2, {"a", "b", "c"}, "MLTS(a ADDS(b c) b)", "ADDS(MLTS(a b b) MLTS(a b c))");

    SINGLE_RULE_TEST(R_MLTS2, {"a", "b", "c"}, "MLTS(a ADDS(b c) ADDS(b c))", "ADDS(MLTS(a b b) MLTS(a b c) MLTS(a b c) MLTS(a c c))");

    return 0;
}

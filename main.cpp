

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

    vector<string> variables = {"a", "b", "c"};
    string inputA = "MLTS(a ADDS(b c) b)";
    string inputB = "MLTS(b a ADDS(c b))";
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
    const NormalTerm<int>* normal_term = static_cast<const NormalTerm<int>*>(term);
    auto sorted = sort_CInstruct(normal_term, bank, ac_symbols);

    cout << sig.term_to_string(term) << endl;
    cout << sig.term_to_string(sorted.first) << endl;
    cout << sorted.second.to_string() << endl;

    auto inverse_instruct = sorted.second.inverse();
    cout << inverse_instruct.to_string() << endl;

    auto applied = apply_CInstruct(sorted.first, inverse_instruct, bank);
    cout << sig.term_to_string(applied) << endl;

    auto termB = parse(sig, bank, inputB);
    const NormalTerm<int>* normal_termB = static_cast<const NormalTerm<int>*>(termB);

    auto instruct = check_C_eq(normal_term, normal_termB, bank, ac_symbols);
    cout << (*instruct).to_string() << endl;

    cout << sig.term_to_string(apply_CInstruct(normal_term, *instruct, bank)) << endl;




    return 0;
}

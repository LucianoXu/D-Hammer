#include <gtest/gtest.h>

#include "diracoq.hpp"

using namespace ualg;
using namespace std;
using namespace diracoq;

TEST(DiracoqSpecialEq, Delta) {
    Prover prover;
    prover.process("Var T : INDEX. Var a : BASIS[T]. Var b : BASIS[T].");
    EXPECT_TRUE(prover.check_eq("DELTA[a, b]", "DELTA[b, a]"));
}

TEST(DiracoqSpecialEq, SUM_SWAP1) {
    unique_var_id = 0;
    Prover* prover = std_prover();

    prover->process(
        R"(
            Var T : INDEX.
            Var f : OTYPE[T, T].
            Var g : OTYPE[T, T].
        )");

    Kernel &kernel = prover->get_kernel();

    auto term = kernel.parse("Tr T (f g)");

    // check result until second rewriting
    auto renamed_res = bound_variable_rename(kernel, term);
    auto temp = pos_rewrite_repeated(kernel, renamed_res, all_rules, nullptr);
    auto expanded_term = variable_expand(kernel, temp);
    temp = pos_rewrite_repeated(kernel, expanded_term, rules, nullptr);

    auto expected_1 = kernel.parse("SUM[USET[T], FUN[@6, BASIS[T], SUM[USET[T], FUN[@7, BASIS[T], MULS[DOT[BRA[@7], MULK[f, KET[@6]]], DOT[BRA[@6], MULK[g, KET[@7]]]]]]]]");

    EXPECT_EQ(temp, expected_1);


    // calculate the bound variables
    auto bound_vars = get_bound_vars(temp);

    // sorting modulo the bound variables
    temp = sort_C_terms(temp, kernel.get_bank(), c_symbols, 
        [&](const Term<int>* a, const Term<int>* b) {
            return comp_modulo_bound_vars(a, b, bound_vars);
        }
    );

    auto expected_2 = kernel.parse("SUM[USET[T], FUN[@6, BASIS[T], SUM[USET[T], FUN[@7, BASIS[T], MULS[DOT[BRA[@7], MULK[f, KET[@6]]], DOT[BRA[@6], MULK[g, KET[@7]]]]]]]]");

    EXPECT_EQ(temp, expected_2);


    // reduce to sum_swap normal form
    temp = sum_swap_normalization(kernel, temp);

    auto expected_3 = kernel.parse("SUM[USET[T], FUN[@7, BASIS[T], SUM[USET[T], FUN[@6, BASIS[T], MULS[DOT[BRA[@7], MULK[f, KET[@6]]], DOT[BRA[@6], MULK[g, KET[@7]]]]]]]]");

    EXPECT_EQ(temp, expected_3);

    delete prover;
}


TEST(DiracoqSpecialEq, SUM_SWAP2) {
    unique_var_id = 0;
    Prover* prover = std_prover();

    prover->process(
        R"(
            Var T : INDEX.
            Var f : OTYPE[T, T].
            Var g : OTYPE[T, T].
        )");

    Kernel &kernel = prover->get_kernel();

    auto term = kernel.parse("Tr T (g f)");

    // check result until second rewriting
    auto renamed_res = bound_variable_rename(kernel, term);
    auto temp = pos_rewrite_repeated(kernel, renamed_res, all_rules, nullptr);
    auto expanded_term = variable_expand(kernel, temp);
    temp = pos_rewrite_repeated(kernel, expanded_term, rules, nullptr);

    auto expected_1 = kernel.parse("SUM[USET[T], FUN[@6, BASIS[T], SUM[USET[T], FUN[@7, BASIS[T], MULS[DOT[BRA[@7], MULK[g, KET[@6]]], DOT[BRA[@6], MULK[f, KET[@7]]]]]]]]");


    EXPECT_EQ(temp, expected_1);


    // calculate the bound variables
    auto bound_vars = get_bound_vars(temp);

    // sorting modulo the bound variables
    temp = sort_C_terms(temp, kernel.get_bank(), c_symbols, 
        [&](const Term<int>* a, const Term<int>* b) {
            return comp_modulo_bound_vars(a, b, bound_vars);
        }
    );

    auto expected_2 = kernel.parse("SUM[USET[T], FUN[@6, BASIS[T], SUM[USET[T], FUN[@7, BASIS[T], MULS[DOT[BRA[@6], MULK[f, KET[@7]]], DOT[BRA[@7], MULK[g, KET[@6]]]]]]]]");

    EXPECT_EQ(temp, expected_2);


    // reduce to sum_swap normal form
    temp = sum_swap_normalization(kernel, temp);

    auto expected_3 = kernel.parse("SUM[USET[T], FUN[@6, BASIS[T], SUM[USET[T], FUN[@7, BASIS[T], MULS[DOT[BRA[@6], MULK[f, KET[@7]]], DOT[BRA[@7], MULK[g, KET[@6]]]]]]]]");

    EXPECT_EQ(temp, expected_3);

    delete prover;
}


TEST(DiracoqSpecialEq, SUM_SWAP3) {
    Prover prover;

    prover.process(
        R"(
            Var T : INDEX.
            Var M : INDEX.
        )");

    EXPECT_TRUE(prover.check_eq(
        "Sum i in USET[T], Sum j in USET[M], 1",
        "Sum j in USET[M], Sum i in USET[T], 1"
    ));
}
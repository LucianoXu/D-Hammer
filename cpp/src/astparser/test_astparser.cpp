#include <gtest/gtest.h>

#include "astparser.hpp"

using namespace astparser;


// Test whether the term are merged correctly
TEST(TestAstParser, Basics) {

    auto expected = AST{"&", {AST{"t", {}}, AST{"s", {}}}};
    auto actual = parse("&[t, s]");

    EXPECT_EQ(expected, actual);
}
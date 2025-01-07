
#include <gtest/gtest.h>

#include "WSTPinterface.hpp"

using namespace std;
using namespace astparser;
using namespace wstp;

WSLINK lp = nullptr;

void init_lp() {
    if (!lp) {
        auto [ep, _lp] = init_and_openlink(MACOS_ARGC, MACOS_ARGV);
        lp = _lp;
    }
}

TEST(TestWSTP, BASIC) {
    init_lp();

    ast_to_WS(lp, parse("Integrate[Sin[x], x]").value());
    AST ast = WS_to_ast(lp);

    EXPECT_EQ(ast.to_string(), "Times[-1, Cos[x]]");
}

TEST(TestWSTP, string2int) {
    init_lp();

    ast_to_WS(lp, parse("Plus[1, 2]").value());
    AST ast = WS_to_ast(lp);

    EXPECT_EQ(ast.to_string(), "3");

    ast_to_WS(lp, parse("Plus[Minus[1], 2]").value());
    ast = WS_to_ast(lp);

    EXPECT_EQ(ast.to_string(), "1");
}

// NOTE: we don't support float number yet.

// TEST(TestWSTP, string2float) {
//     init_lp();

//     ast_to_WS(lp, parse("Plus[1.1, 2.0]").value());
//     AST ast = WS_to_ast(lp);

//     EXPECT_EQ(ast.to_string(), "3.1");

//     ast_to_WS(lp, parse("Plus[-1.0, 2.0]").value());
//     ast = WS_to_ast(lp);

//     EXPECT_EQ(ast.to_string(), "1.0");
// }

TEST(TestWSTP, string2complex) {
    init_lp();

    ast_to_WS(lp, parse("Times[I, I]").value());
    AST ast = WS_to_ast(lp);

    EXPECT_EQ(ast.to_string(), "-1");
}
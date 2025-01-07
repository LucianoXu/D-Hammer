
#include <gtest/gtest.h>

#include "WSTPinterface.hpp"

using namespace std;
using namespace astparser;
using namespace wstp;


TEST(TestWSTP, BASIC) {
        
    const char* args[] = {
        "-linkmode", "launch",
        "-linkname", "\"/Applications/Wolfram Engine.app/Contents/Resources/Wolfram Player.app/Contents/MacOS/WolframKernel\" -wstp"
    };
    int argc = sizeof(args) / sizeof(args[0]);
    char** argv = const_cast<char**>(args);
    
    init_and_openlink(argc, argv);

    ast_to_WS(lp, parse("Integrate[Sin[x], x]").value());
    AST ast = WS_to_ast(lp);

    EXPECT_EQ(ast.to_string(), "Times[-1, Cos[x]]");
}
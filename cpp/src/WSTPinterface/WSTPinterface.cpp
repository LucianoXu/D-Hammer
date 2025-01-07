
#include "WSTPinterface.hpp"
#include <functional>
#include <regex>

namespace wstp {
    using namespace std;
    using namespace astparser;

    // define the arguments for the Wolfram Engine (MacOS)
    const char* MacOS_args[] = {
        "-linkmode", "launch",
        "-linkname", "\"/Applications/Wolfram Engine.app/Contents/Resources/Wolfram Player.app/Contents/MacOS/WolframKernel\" -wstp"
    }; 
    const int MACOS_ARGC = sizeof(MacOS_args) / sizeof(MacOS_args[0]);
    char** const MACOS_ARGV = const_cast<char** const>(MacOS_args);



    vector<pair<WSENV, WSLINK>> links;

    // The closing of the links is not working properly (at least during tests), so I'm commenting it out.

    // const int i = atexit([](){
    //     for (auto [ep, lp] : links) {
    //         if (lp) WSClose(lp);
    //         if (ep) WSDeinitialize(ep);
    //     }
    // });

    pair<WSENV, WSLINK> init_and_openlink(int argc, char* argv[])
    {

        // static wolfram engine environment and link
        WSENV ep = (WSENV)0;
        WSLINK lp = (WSLINK)0;
        int err;

        ep =  WSInitialize((WSParametersPointer)0);
        if (ep == (WSENV)0) exit(1);

        lp = WSOpenArgv(ep, argv, argv + argc, &err);
        if (lp == (WSLINK)0) exit(2);

        links.push_back({ep, lp});

        return {ep, lp};
    }

    // a regex to identify integers
    const regex int_regex("^[+-]?[0-9]+$");

    void _ast_to_WS(WSLINK lp, const AST& ast) {
        if (ast.children.size() == 0) {

            // if the head can be converted into an integer
            if (regex_match(ast.head, int_regex)) {
                WSPutInteger(lp, strtol(ast.head.c_str(), nullptr, 10));
            }
            else {
                WSPutSymbol(lp, ast.head.c_str());
            }
        }
        else {
            WSPutFunction(lp, ast.head.c_str(), ast.children.size());
            for (const auto& child : ast.children) {
                _ast_to_WS(lp, child);
            }
        }
    }

    void ast_to_WS(WSLINK lp, const AST& ast) {
        WSPutFunction(lp, "EvaluatePacket", 1L);
        _ast_to_WS(lp, ast);
        WSEndPacket(lp);
    }


    AST _WS_to_ast(WSLINK lp) {
        const char* sp;
        int countp;
        int int_res;

        switch (int type = WSGetNext(lp)) {
            case WSTKFUNC:
                WSGetFunction(lp, &sp, &countp);
                if (countp == 0) {
                    string head = sp;
                    return {head, {}};
                } 
                else {
                    vector<AST> args;
                    for (int i = 0; i < countp; i++) {
                        args.push_back(_WS_to_ast(lp));
                    }
                    return {sp, args};
                }
                break;
            
            case WSTKSYM:
                WSGetSymbol(lp, &sp);
                return {sp, {}};
                break;

            case WSTKINT:
                WSGetInteger(lp, &int_res);
                return {to_string(int_res), {}};
                break;

            default:
                throw runtime_error("Unknown type: " + to_string(type));

        }
    }


    AST WS_to_ast(WSLINK lp) {
        // wait for the result
        int pkt;
        while((pkt = WSNextPacket(lp), pkt) && pkt != RETURNPKT) {
            WSNewPacket(lp);
            if (WSError(lp)) {
                fprintf( stderr, "Error detected by WSTP: %s.\n",
                    WSErrorMessage(lp));
                exit(3);
            }
        }

        return _WS_to_ast(lp);
    }


} // namespace wstp

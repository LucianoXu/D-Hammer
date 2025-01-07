
#include "WSTPinterface.hpp"

namespace wstp {
    using namespace std;
    using namespace astparser;

    // static wolfram engine environment and link
    WSENV ep = (WSENV)0;
    WSLINK lp = (WSLINK)0;


    void init_and_openlink(int argc, char* argv[])
    {
        int err;

        ep =  WSInitialize((WSParametersPointer)0);
        if (ep == (WSENV)0) exit(1);
        atexit([](){if (ep) WSDeinitialize(ep);});

        lp = WSOpenArgv(ep, argv, argv + argc, &err);
        if (lp == (WSLINK)0) exit(2);
        atexit([](){if (lp) WSClose(lp);});	
    }

    void _ast_to_WS(WSLINK lp, const AST& ast) {
        if (ast.children.size() == 0) {
            WSPutSymbol(lp, ast.head.c_str());
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

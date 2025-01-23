#include "dirace.hpp"

namespace dirace {
    using namespace std;
    using namespace ualg;
    using namespace astparser;


    class DIRACEBuilder : public DIRACEBaseListener {
    public:
        DIRACEBuilder();

        AST get_root();

        // expr
        void exitCmdSeq(DIRACEParser::CmdSeqContext *ctx) override;
        void exitFromTerm(DIRACEParser::FromTermContext *ctx) override;

        // cmd
        void exitDefinition0(DIRACEParser::Definition0Context *ctx) override;
        void exitDefinition1(DIRACEParser::Definition1Context *ctx) override;
        void exitAssum(DIRACEParser::AssumContext *ctx) override;
        void exitCheck(DIRACEParser::CheckContext *ctx) override;
        void exitShow(DIRACEParser::ShowContext *ctx) override;
        void exitShowAll(DIRACEParser::ShowAllContext *ctx) override;
        void exitNormalize(DIRACEParser::NormalizeContext *ctx) override;
        void exitNormalizeTraced(DIRACEParser::NormalizeTracedContext *ctx) override;
        void exitCheckEq(DIRACEParser::CheckEqContext *ctx) override;

        // term
        void exitBra(DIRACEParser::BraContext *ctx) override;
        void exitKet(DIRACEParser::KetContext *ctx) override;
        void exitDelta(DIRACEParser::DeltaContext *ctx) override;
        void exitPair(DIRACEParser::PairContext *ctx) override;
        void exitAdj(DIRACEParser::AdjContext *ctx) override;
        void exitConj(DIRACEParser::ConjContext *ctx) override;
        void exitScr(DIRACEParser::ScrContext *ctx) override;
        void exitCompo(DIRACEParser::CompoContext *ctx) override;
        void exitStar(DIRACEParser::StarContext *ctx) override;
        void exitAdd(DIRACEParser::AddContext *ctx) override;
        void exitArrow(DIRACEParser::ArrowContext *ctx) override;
        void exitSum(DIRACEParser::SumContext *ctx) override;
        void exitIdx(DIRACEParser::IdxContext *ctx) override;
        void exitFun(DIRACEParser::FunContext *ctx) override;
        void exitForall(DIRACEParser::ForallContext *ctx) override;

        void exitZeroK(DIRACEParser::ZeroKContext *ctx) override;
        void exitZeroB(DIRACEParser::ZeroBContext *ctx) override;
        void exitZeroO(DIRACEParser::ZeroOContext *ctx) override;
        void exitZeroD(DIRACEParser::ZeroDContext *ctx) override;
        void exitOneO(DIRACEParser::OneOContext *ctx) override;

        void exitSubscript1(DIRACEParser::Subscript1Context *ctx) override;
        void exitSubscript2(DIRACEParser::Subscript2Context *ctx) override;

        void exitBasis0(DIRACEParser::Basis0Context *ctx) override;
        void exitBasis1(DIRACEParser::Basis1Context *ctx) override;

        void exitRSet(DIRACEParser::RSetContext *ctx) override;
        void exitEmptyRSet(DIRACEParser::EmptyRSetContext *ctx) override;

        void exitEmptyApplication(DIRACEParser::EmptyApplicationContext *ctx) override;
        void exitApplication(DIRACEParser::ApplicationContext *ctx) override;
        void exitParen(DIRACEParser::ParenContext *ctx) override;
        void exitIdentifier(DIRACEParser::IdentifierContext *ctx) override;


    private:
        std::stack<AST> node_stack;
    };

    DIRACEBuilder::DIRACEBuilder() {}

    AST DIRACEBuilder::get_root() {
        if (!node_stack.empty()) {
            return std::move(node_stack.top());
        }
        
        throw std::runtime_error("No root node found.");
    }

    /////////////////////////////////
    // expr

    // Handle Command Sequence node
    void DIRACEBuilder::exitCmdSeq(DIRACEParser::CmdSeqContext *ctx) {
        std::vector<AST> commands;

        // Pop commands from the stack in reverse order
        for (int i = 0; i < ctx->cmd().size(); ++i) {
            commands.insert(commands.begin(), node_stack.top());
            node_stack.pop();
        }

        // Push the sequence as a single AST node
        node_stack.push(AST{"GROUP", std::move(commands)});
    }

    // Handle From Term node
    void DIRACEBuilder::exitFromTerm(DIRACEParser::FromTermContext *ctx) {
        // Pop the expression node from the stack
        AST from_term = std::move(node_stack.top());
        node_stack.pop();

        node_stack.push(std::move(from_term));
    }


    ///////////////////////////////////////////
    // cmd

    // Handle Definition0 node
    void DIRACEBuilder::exitDefinition0(DIRACEParser::Definition0Context *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Pop the expression node from the stack
        AST definition_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the definition node
        node_stack.push(AST{"DEF", {AST{def_name, {}}, std::move(definition_body)}});
    }

    // Handle Definition1 node
    void DIRACEBuilder::exitDefinition1(DIRACEParser::Definition1Context *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Pop the type and body expressions in reverse order
        AST definition_type = std::move(node_stack.top());
        node_stack.pop();
        AST definition_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the definition node
        node_stack.push(
            AST{"DEF", {AST{def_name, {}}, std::move(definition_body), std::move(definition_type)}}
        );
    }

    void DIRACEBuilder::exitAssum(DIRACEParser::AssumContext *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Pop the body expression
        AST definition_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the assumption node
        node_stack.push(AST{"VAR", {AST{def_name, {}}, std::move(definition_body)}});
    }

    void DIRACEBuilder::exitCheck(DIRACEParser::CheckContext *ctx) {
        // Pop the checked term
        AST check_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the check node
        node_stack.push(AST{"CHECK", {std::move(check_body)}});
    }

    void DIRACEBuilder::exitShow(DIRACEParser::ShowContext *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Create and push the show node
        node_stack.push(AST{"SHOW", {AST{def_name, {}}}});
    }

    void DIRACEBuilder::exitShowAll(DIRACEParser::ShowAllContext *ctx) {
        // Create and push the show all node
        node_stack.push(AST{"SHOWALL", {}});
    }

    void DIRACEBuilder::exitNormalize(DIRACEParser::NormalizeContext *ctx) {
        // Pop the expression node from the stack
        AST normalize_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the normalize node
        node_stack.push(AST{"NORMALIZE", {std::move(normalize_body)}});
    }

    void DIRACEBuilder::exitNormalizeTraced(DIRACEParser::NormalizeTracedContext *ctx) {
        // Pop the expression node from the stack
        AST normalize_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the normalize node
        node_stack.push(AST{"NORMALIZE", {std::move(normalize_body), AST{"TRACE", {}}}});
    }

    void DIRACEBuilder::exitCheckEq(DIRACEParser::CheckEqContext *ctx) {
        // Pop the two expression nodes from the stack
        AST rhs = std::move(node_stack.top());
        node_stack.pop();
        AST lhs = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the check equality node
        node_stack.push(AST{"CHECKEQ", {std::move(lhs), std::move(rhs)}});
    }

    ///////////////////////////////////////////
    // term

    void DIRACEBuilder::exitBra(DIRACEParser::BraContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Bra node
        node_stack.push(AST{"BRA", {std::move(term)}});
    }

    void DIRACEBuilder::exitKet(DIRACEParser::KetContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Ket node
        node_stack.push(AST{"KET", {std::move(term)}});
    }


    void DIRACEBuilder::exitDelta(DIRACEParser::DeltaContext *ctx) {
        // get t2
        AST t2 = std::move(node_stack.top());
        node_stack.pop();

        // get t1
        AST t1 = std::move(node_stack.top());
        node_stack.pop();

        // push Delta node
        node_stack.push(AST{"DELTA", {std::move(t1), std::move(t2)}});
    }

    // (e1, e2)
    void DIRACEBuilder::exitPair(DIRACEParser::PairContext *ctx) {
        // get e2
        AST e2 = std::move(node_stack.top());
        node_stack.pop();

        // get e1
        AST e1 = std::move(node_stack.top());
        node_stack.pop();
        
        // push Pair node
        node_stack.push(AST{"PAIR", {std::move(e1), std::move(e2)}});
    }


    void DIRACEBuilder::exitAdj(DIRACEParser::AdjContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Adj node
        node_stack.push(AST{"ADJ", {std::move(term)}});
    }

    void DIRACEBuilder::exitConj(DIRACEParser::ConjContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Conj node
        node_stack.push(AST{"Conjugate", {std::move(term)}});
    }

    void DIRACEBuilder::exitScr(DIRACEParser::ScrContext *ctx) {
        // get dirac term
        AST dirac_term = std::move(node_stack.top());
        node_stack.pop();

        // get scalar term
        AST scalar_term = std::move(node_stack.top());
        node_stack.pop();

        // push Scr node
        node_stack.push(AST{"SCR", {std::move(scalar_term), std::move(dirac_term)}});
    }

    // COMPO(a b)
    void DIRACEBuilder::exitCompo(DIRACEParser::CompoContext *ctx) {
        // get b
        AST b = std::move(node_stack.top());
        node_stack.pop();

        // get a
        AST a = std::move(node_stack.top());
        node_stack.pop();
        
        // push Compo node
        node_stack.push(AST{"COMPO", {std::move(a), std::move(b)}});
    }

    // STAR(a b)
    void DIRACEBuilder::exitStar(DIRACEParser::StarContext *ctx) {
        // get b
        AST b = std::move(node_stack.top());
        node_stack.pop();

        // get a
        AST a = std::move(node_stack.top());
        node_stack.pop();
        
        // push Star node
        node_stack.push(AST{"STAR", {std::move(a), std::move(b)}});
    }

    // ADDG(a b)
    void DIRACEBuilder::exitAdd(DIRACEParser::AddContext *ctx) {
        // get b
        AST b = std::move(node_stack.top());
        node_stack.pop();

        // get a
        AST a = std::move(node_stack.top());
        node_stack.pop();
        
        // push Add node
        node_stack.push(AST{"ADDG", {std::move(a), std::move(b)}});
    }

    // T1 -> T2
    void DIRACEBuilder::exitArrow(DIRACEParser::ArrowContext *ctx) {
        // get T2
        AST t2 = std::move(node_stack.top());
        node_stack.pop();

        // get T1
        AST t1 = std::move(node_stack.top());
        node_stack.pop();
        
        // push ARROW node
        node_stack.push(AST{"ARROW", {std::move(t1), std::move(t2)}});
    }

        void DIRACEBuilder::exitSum(DIRACEParser::SumContext *ctx) {
        // get the body
        AST body = std::move(node_stack.top());
        node_stack.pop();

        // get the set
        AST set = std::move(node_stack.top());
        node_stack.pop();

        // get the name
        std::string name = ctx->ID()->getText();

        // push Sum node
        node_stack.push(AST{"SSUM", 
            {
                AST(name, {}), std::move(set), std::move(body)
            }
        });
    }

    void DIRACEBuilder::exitIdx(DIRACEParser::IdxContext *ctx) {
        // get the body
        AST body = std::move(node_stack.top());
        node_stack.pop();

        // get the name
        std::string name = ctx->ID()->getText();

        // push Idx node
        node_stack.push(AST{"IDX", {AST(name, {}), std::move(body)}});
    }

    void DIRACEBuilder::exitFun(DIRACEParser::FunContext *ctx) {
        // get the body
        AST body = std::move(node_stack.top());
        node_stack.pop();

        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // get the name
        std::string name = ctx->ID()->getText();

        // push Fun node
        node_stack.push(AST{"FUN", {AST(name, {}), std::move(type), std::move(body)}});
    }


    void DIRACEBuilder::exitForall(DIRACEParser::ForallContext *ctx) {
        // get the body
        AST body = std::move(node_stack.top());
        node_stack.pop();

        // get the name
        std::string name = ctx->ID()->getText();

        // push FORALL node
        node_stack.push(AST{"FORALL", {AST(name, {}), std::move(body)}});
    }

    void DIRACEBuilder::exitZeroK(DIRACEParser::ZeroKContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroK node
        node_stack.push(AST{"ZEROK", {std::move(type)}});
    }

    void DIRACEBuilder::exitZeroB(DIRACEParser::ZeroBContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroB node
        node_stack.push(AST{"ZEROB", {std::move(type)}});
    }

    void DIRACEBuilder::exitZeroO(DIRACEParser::ZeroOContext *ctx) {
        // get the type
        AST type2 = std::move(node_stack.top());
        node_stack.pop();
        AST type1 = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroO node
        node_stack.push(AST{"ZEROO", {std::move(type1), std::move(type2)}});
    }

    void DIRACEBuilder::exitZeroD(DIRACEParser::ZeroDContext *ctx) {
        // get the register
        AST reg2 = std::move(node_stack.top());
        node_stack.pop();
        AST reg1 = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroD node
        node_stack.push(AST{"ZEROD", {std::move(reg1), std::move(reg2)}});
    }

    void DIRACEBuilder::exitOneO(DIRACEParser::OneOContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push OneO node
        node_stack.push(AST{"ONEO", {std::move(type)}});
    }

    void DIRACEBuilder::exitBasis0(DIRACEParser::Basis0Context *ctx) {
        node_stack.push(AST{"BASIS0", {}});
    }

    void DIRACEBuilder::exitBasis1(DIRACEParser::Basis1Context *ctx) {
        node_stack.push(AST{"BASIS1", {}});
    }

    void DIRACEBuilder::exitSubscript1(DIRACEParser::Subscript1Context *ctx) {
        // get the register
        AST reg = std::move(node_stack.top());
        node_stack.pop();

        // get the term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Subscript1 node
        node_stack.push(AST{"SUBS", {std::move(term), std::move(reg)}});
    }

    void DIRACEBuilder::exitSubscript2(DIRACEParser::Subscript2Context *ctx) {
        // get the register2
        AST reg2 = std::move(node_stack.top());
        node_stack.pop();

        // get the register1
        AST reg1 = std::move(node_stack.top());
        node_stack.pop();

        // get the term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Subscript2 node
        node_stack.push(AST{"SUBS", {std::move(term), std::move(reg1), std::move(reg2)}});
    }


    // '{' ID (',' ID)* '}'
    void DIRACEBuilder::exitRSet(DIRACEParser::RSetContext *ctx) {
        // 2. Collect all IDs in reverse order
        vector<AST> ids;
        for (int i = 0; i < ctx->ID().size(); ++i) {
            ids.push_back(AST(ctx->ID(i)->getText()));
        }

        // 3. Create and push the RSet node
        node_stack.push(AST{"RSET", std::move(ids)});

    }


    void DIRACEBuilder::exitEmptyRSet(DIRACEParser::EmptyRSetContext *ctx) {
        node_stack.push(AST{"RSET"});
    }
        

    void DIRACEBuilder::exitEmptyApplication(DIRACEParser::EmptyApplicationContext *ctx) {
        node_stack.push(AST{ctx->ID()->getText(), {}});
    }

    // Handle Application node
    void DIRACEBuilder::exitApplication(DIRACEParser::ApplicationContext *ctx) {
        // 1. Get the function identifier (ID)
        std::string function_name = ctx->ID()->getText();

        // 2. Collect all arguments in reverse order
        std::vector<AST> arguments;
        for (int i = 0; i < ctx->expr().size(); ++i) {
            arguments.insert(arguments.begin(), node_stack.top());
            node_stack.pop();
        }

        // 3. Create and push the application node
        node_stack.push(AST{function_name, std::move(arguments)});
    }

    void DIRACEBuilder::exitParen(DIRACEParser::ParenContext *ctx) {
        // pass
    }

    // Handle Identifier node
    void DIRACEBuilder::exitIdentifier(DIRACEParser::IdentifierContext *ctx) {
        std::string identifier_name = ctx->getText();
        node_stack.push(AST{identifier_name, {}});
    }


    ///////////////////////////////////////////
    // parsing

    void printTokenStream(antlr4::CommonTokenStream &tokens) {
        for (auto token : tokens.getTokens()) {
            std::cout << "Token Text: " << token->getText() << ", "
                    << "TYPE: " << token->getType() << ", "
                    << "Line: " << token->getLine() << ", "
                    << "Char Position: " << token->getCharPositionInLine() << std::endl;
        }
    }

    // Parse function
    std::optional<AST> parse(const std::string& code, bool debug) {
        using namespace antlr4;

        ANTLRInputStream input(code);
        DIRACELexer lexer(&input);
        CommonTokenStream tokens(&lexer);

        tokens.fill();

        // Print the token stream
        if (debug) printTokenStream(tokens);

        DIRACEParser parser(&tokens);

        // Enable parser trace if debug is true
        if (debug) parser.setTrace(true);

        tree::ParseTree *tree = parser.expr(); // Start from 'expr' rule

        // Create the AST builder
        DIRACEBuilder treeBuilder;

        // CHECK for syntax errors
        if (parser.getNumberOfSyntaxErrors() == 0) {
            antlr4::tree::ParseTreeWalker::DEFAULT.walk(&treeBuilder, tree);

            // Retrieve the root of the AST
            return treeBuilder.get_root();
        } else {
            return std::nullopt;
        }
    }

} // namespace dirace
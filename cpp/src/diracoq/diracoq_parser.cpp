#include "diracoq.hpp"

namespace diracoq {
    using namespace std;
    using namespace ualg;
    using namespace astparser;


    class DIRACOQBuilder : public DIRACOQBaseListener {
    public:
        DIRACOQBuilder();

        AST get_root();

        // expr
        void exitCmdSeq(DIRACOQParser::CmdSeqContext *ctx) override;
        void exitFromTerm(DIRACOQParser::FromTermContext *ctx) override;

        // cmd
        void exitDefinition0(DIRACOQParser::Definition0Context *ctx) override;
        void exitDefinition1(DIRACOQParser::Definition1Context *ctx) override;
        void exitAssum(DIRACOQParser::AssumContext *ctx) override;
        void exitCheck(DIRACOQParser::CheckContext *ctx) override;
        void exitShow(DIRACOQParser::ShowContext *ctx) override;
        void exitShowAll(DIRACOQParser::ShowAllContext *ctx) override;
        void exitNormalize(DIRACOQParser::NormalizeContext *ctx) override;
        void exitNormalizeTraced(DIRACOQParser::NormalizeTracedContext *ctx) override;
        void exitCheckEq(DIRACOQParser::CheckEqContext *ctx) override;

        // term
        void exitBra(DIRACOQParser::BraContext *ctx) override;
        void exitKet(DIRACOQParser::KetContext *ctx) override;
        void exitDelta(DIRACOQParser::DeltaContext *ctx) override;
        void exitPair(DIRACOQParser::PairContext *ctx) override;
        void exitAdj(DIRACOQParser::AdjContext *ctx) override;
        void exitConj(DIRACOQParser::ConjContext *ctx) override;
        void exitScr(DIRACOQParser::ScrContext *ctx) override;
        void exitCompo(DIRACOQParser::CompoContext *ctx) override;
        void exitStar(DIRACOQParser::StarContext *ctx) override;
        void exitAdd(DIRACOQParser::AddContext *ctx) override;
        void exitArrow(DIRACOQParser::ArrowContext *ctx) override;
        void exitSum(DIRACOQParser::SumContext *ctx) override;
        void exitIdx(DIRACOQParser::IdxContext *ctx) override;
        void exitFun(DIRACOQParser::FunContext *ctx) override;
        void exitForall(DIRACOQParser::ForallContext *ctx) override;

        void exitZeroK(DIRACOQParser::ZeroKContext *ctx) override;
        void exitZeroB(DIRACOQParser::ZeroBContext *ctx) override;
        void exitZeroO(DIRACOQParser::ZeroOContext *ctx) override;
        void exitOneO(DIRACOQParser::OneOContext *ctx) override;

        void exitApplication(DIRACOQParser::ApplicationContext *ctx) override;
        void exitParen(DIRACOQParser::ParenContext *ctx) override;
        void exitIdentifier(DIRACOQParser::IdentifierContext *ctx) override;


    private:
        std::stack<AST> node_stack;
    };

    DIRACOQBuilder::DIRACOQBuilder() {}

    AST DIRACOQBuilder::get_root() {
        if (!node_stack.empty()) {
            return std::move(node_stack.top());
        }
        
        throw std::runtime_error("No root node found.");
    }

    /////////////////////////////////
    // expr

    // Handle Command Sequence node
    void DIRACOQBuilder::exitCmdSeq(DIRACOQParser::CmdSeqContext *ctx) {
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
    void DIRACOQBuilder::exitFromTerm(DIRACOQParser::FromTermContext *ctx) {
        // Pop the expression node from the stack
        AST from_term = std::move(node_stack.top());
        node_stack.pop();

        node_stack.push(std::move(from_term));
    }


    ///////////////////////////////////////////
    // cmd

    // Handle Definition0 node
    void DIRACOQBuilder::exitDefinition0(DIRACOQParser::Definition0Context *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Pop the expression node from the stack
        AST definition_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the definition node
        node_stack.push(AST{"DEF", {AST{def_name, {}}, std::move(definition_body)}});
    }

    // Handle Definition1 node
    void DIRACOQBuilder::exitDefinition1(DIRACOQParser::Definition1Context *ctx) {
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

    void DIRACOQBuilder::exitAssum(DIRACOQParser::AssumContext *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Pop the body expression
        AST definition_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the assumption node
        node_stack.push(AST{"VAR", {AST{def_name, {}}, std::move(definition_body)}});
    }

    void DIRACOQBuilder::exitCheck(DIRACOQParser::CheckContext *ctx) {
        // Pop the checked term
        AST check_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the check node
        node_stack.push(AST{"CHECK", {std::move(check_body)}});
    }

    void DIRACOQBuilder::exitShow(DIRACOQParser::ShowContext *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Create and push the show node
        node_stack.push(AST{"SHOW", {AST{def_name, {}}}});
    }

    void DIRACOQBuilder::exitShowAll(DIRACOQParser::ShowAllContext *ctx) {
        // Create and push the show all node
        node_stack.push(AST{"SHOWALL", {}});
    }

    void DIRACOQBuilder::exitNormalize(DIRACOQParser::NormalizeContext *ctx) {
        // Pop the expression node from the stack
        AST normalize_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the normalize node
        node_stack.push(AST{"NORMALIZE", {std::move(normalize_body)}});
    }

    void DIRACOQBuilder::exitNormalizeTraced(DIRACOQParser::NormalizeTracedContext *ctx) {
        // Pop the expression node from the stack
        AST normalize_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the normalize node
        node_stack.push(AST{"NORMALIZE", {std::move(normalize_body), AST{"TRACE", {}}}});
    }

    void DIRACOQBuilder::exitCheckEq(DIRACOQParser::CheckEqContext *ctx) {
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

    void DIRACOQBuilder::exitBra(DIRACOQParser::BraContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Bra node
        node_stack.push(AST{"BRA", {std::move(term)}});
    }

    void DIRACOQBuilder::exitKet(DIRACOQParser::KetContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Ket node
        node_stack.push(AST{"KET", {std::move(term)}});
    }


    void DIRACOQBuilder::exitDelta(DIRACOQParser::DeltaContext *ctx) {
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
    void DIRACOQBuilder::exitPair(DIRACOQParser::PairContext *ctx) {
        // get e2
        AST e2 = std::move(node_stack.top());
        node_stack.pop();

        // get e1
        AST e1 = std::move(node_stack.top());
        node_stack.pop();
        
        // push Pair node
        node_stack.push(AST{"PAIR", {std::move(e1), std::move(e2)}});
    }


    void DIRACOQBuilder::exitAdj(DIRACOQParser::AdjContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Adj node
        node_stack.push(AST{"ADJ", {std::move(term)}});
    }

    void DIRACOQBuilder::exitConj(DIRACOQParser::ConjContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Conj node
        node_stack.push(AST{"CONJ", {std::move(term)}});
    }

    void DIRACOQBuilder::exitScr(DIRACOQParser::ScrContext *ctx) {
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
    void DIRACOQBuilder::exitCompo(DIRACOQParser::CompoContext *ctx) {
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
    void DIRACOQBuilder::exitStar(DIRACOQParser::StarContext *ctx) {
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
    void DIRACOQBuilder::exitAdd(DIRACOQParser::AddContext *ctx) {
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
    void DIRACOQBuilder::exitArrow(DIRACOQParser::ArrowContext *ctx) {
        // get T2
        AST t2 = std::move(node_stack.top());
        node_stack.pop();

        // get T1
        AST t1 = std::move(node_stack.top());
        node_stack.pop();
        
        // push ARROW node
        node_stack.push(AST{"ARROW", {std::move(t1), std::move(t2)}});
    }

        void DIRACOQBuilder::exitSum(DIRACOQParser::SumContext *ctx) {
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

    void DIRACOQBuilder::exitIdx(DIRACOQParser::IdxContext *ctx) {
        // get the body
        AST body = std::move(node_stack.top());
        node_stack.pop();

        // get the name
        std::string name = ctx->ID()->getText();

        // push Idx node
        node_stack.push(AST{"IDX", {AST(name, {}), std::move(body)}});
    }

    void DIRACOQBuilder::exitFun(DIRACOQParser::FunContext *ctx) {
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


    void DIRACOQBuilder::exitForall(DIRACOQParser::ForallContext *ctx) {
        // get the body
        AST body = std::move(node_stack.top());
        node_stack.pop();

        // get the name
        std::string name = ctx->ID()->getText();

        // push FORALL node
        node_stack.push(AST{"FORALL", {AST(name, {}), std::move(body)}});
    }

    void DIRACOQBuilder::exitZeroK(DIRACOQParser::ZeroKContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroK node
        node_stack.push(AST{"ZEROK", {std::move(type)}});
    }

    void DIRACOQBuilder::exitZeroB(DIRACOQParser::ZeroBContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroB node
        node_stack.push(AST{"ZEROB", {std::move(type)}});
    }

    void DIRACOQBuilder::exitZeroO(DIRACOQParser::ZeroOContext *ctx) {
        // get the type
        AST type2 = std::move(node_stack.top());
        node_stack.pop();
        AST type1 = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroO node
        node_stack.push(AST{"ZEROO", {std::move(type1), std::move(type2)}});
    }

    void DIRACOQBuilder::exitOneO(DIRACOQParser::OneOContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push OneO node
        node_stack.push(AST{"ONEO", {std::move(type)}});
    }
        


    // Handle Application node
    void DIRACOQBuilder::exitApplication(DIRACOQParser::ApplicationContext *ctx) {
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

    void DIRACOQBuilder::exitParen(DIRACOQParser::ParenContext *ctx) {
        // pass
    }

    // Handle Identifier node
    void DIRACOQBuilder::exitIdentifier(DIRACOQParser::IdentifierContext *ctx) {
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
        DIRACOQLexer lexer(&input);
        CommonTokenStream tokens(&lexer);

        tokens.fill();

        // Print the token stream
        if (debug) printTokenStream(tokens);

        DIRACOQParser parser(&tokens);

        // Enable parser trace if debug is true
        if (debug) parser.setTrace(true);

        tree::ParseTree *tree = parser.expr(); // Start from 'expr' rule

        // Create the AST builder
        DIRACOQBuilder treeBuilder;

        // CHECK for syntax errors
        if (parser.getNumberOfSyntaxErrors() == 0) {
            antlr4::tree::ParseTreeWalker::DEFAULT.walk(&treeBuilder, tree);

            // Retrieve the root of the AST
            return treeBuilder.get_root();
        } else {
            return std::nullopt;
        }
    }

} // namespace diracoq
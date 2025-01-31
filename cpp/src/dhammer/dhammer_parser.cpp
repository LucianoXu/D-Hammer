#include "dhammer.hpp"

namespace dhammer {
    using namespace std;
    using namespace ualg;
    using namespace astparser;


    class DHAMMERBuilder : public DHAMMERBaseListener {
    public:
        DHAMMERBuilder();

        AST get_root();

        // expr
        void exitCmdSeq(DHAMMERParser::CmdSeqContext *ctx) override;
        void exitFromTerm(DHAMMERParser::FromTermContext *ctx) override;

        // cmd
        void exitDefinition0(DHAMMERParser::Definition0Context *ctx) override;
        void exitDefinition1(DHAMMERParser::Definition1Context *ctx) override;
        void exitAssum(DHAMMERParser::AssumContext *ctx) override;
        void exitCheck(DHAMMERParser::CheckContext *ctx) override;
        void exitShow(DHAMMERParser::ShowContext *ctx) override;
        void exitShowAll(DHAMMERParser::ShowAllContext *ctx) override;
        void exitNormalize(DHAMMERParser::NormalizeContext *ctx) override;
        void exitNormalizeTraced(DHAMMERParser::NormalizeTracedContext *ctx) override;
        void exitCheckEq(DHAMMERParser::CheckEqContext *ctx) override;

        // term
        void exitBra(DHAMMERParser::BraContext *ctx) override;
        void exitKet(DHAMMERParser::KetContext *ctx) override;
        void exitDelta(DHAMMERParser::DeltaContext *ctx) override;
        void exitPair(DHAMMERParser::PairContext *ctx) override;
        void exitAdj(DHAMMERParser::AdjContext *ctx) override;
        void exitConj(DHAMMERParser::ConjContext *ctx) override;
        void exitScr(DHAMMERParser::ScrContext *ctx) override;
        void exitCompo(DHAMMERParser::CompoContext *ctx) override;
        void exitStar(DHAMMERParser::StarContext *ctx) override;
        void exitAdd(DHAMMERParser::AddContext *ctx) override;
        void exitArrow(DHAMMERParser::ArrowContext *ctx) override;
        void exitSum(DHAMMERParser::SumContext *ctx) override;
        void exitIdx(DHAMMERParser::IdxContext *ctx) override;
        void exitFun(DHAMMERParser::FunContext *ctx) override;
        void exitForall(DHAMMERParser::ForallContext *ctx) override;

        void exitZeroK(DHAMMERParser::ZeroKContext *ctx) override;
        void exitZeroB(DHAMMERParser::ZeroBContext *ctx) override;
        void exitZeroO(DHAMMERParser::ZeroOContext *ctx) override;
        void exitZeroD(DHAMMERParser::ZeroDContext *ctx) override;
        void exitOneO(DHAMMERParser::OneOContext *ctx) override;

        void exitSubscript1(DHAMMERParser::Subscript1Context *ctx) override;
        void exitSubscript2(DHAMMERParser::Subscript2Context *ctx) override;

        void exitBasis0(DHAMMERParser::Basis0Context *ctx) override;
        void exitBasis1(DHAMMERParser::Basis1Context *ctx) override;

        void exitRSet(DHAMMERParser::RSetContext *ctx) override;
        void exitEmptyRSet(DHAMMERParser::EmptyRSetContext *ctx) override;

        void exitEmptyApplication(DHAMMERParser::EmptyApplicationContext *ctx) override;
        void exitApplication(DHAMMERParser::ApplicationContext *ctx) override;
        void exitParen(DHAMMERParser::ParenContext *ctx) override;
        void exitIdentifier(DHAMMERParser::IdentifierContext *ctx) override;


    private:
        std::stack<AST> node_stack;
    };

    DHAMMERBuilder::DHAMMERBuilder() {}

    AST DHAMMERBuilder::get_root() {
        if (!node_stack.empty()) {
            return std::move(node_stack.top());
        }
        
        throw std::runtime_error("No root node found.");
    }

    /////////////////////////////////
    // expr

    // Handle Command Sequence node
    void DHAMMERBuilder::exitCmdSeq(DHAMMERParser::CmdSeqContext *ctx) {
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
    void DHAMMERBuilder::exitFromTerm(DHAMMERParser::FromTermContext *ctx) {
        // Pop the expression node from the stack
        AST from_term = std::move(node_stack.top());
        node_stack.pop();

        node_stack.push(std::move(from_term));
    }


    ///////////////////////////////////////////
    // cmd

    // Handle Definition0 node
    void DHAMMERBuilder::exitDefinition0(DHAMMERParser::Definition0Context *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Pop the expression node from the stack
        AST definition_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the definition node
        node_stack.push(AST{"DEF", {AST{def_name, {}}, std::move(definition_body)}});
    }

    // Handle Definition1 node
    void DHAMMERBuilder::exitDefinition1(DHAMMERParser::Definition1Context *ctx) {
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

    void DHAMMERBuilder::exitAssum(DHAMMERParser::AssumContext *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Pop the body expression
        AST definition_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the assumption node
        node_stack.push(AST{"VAR", {AST{def_name, {}}, std::move(definition_body)}});
    }

    void DHAMMERBuilder::exitCheck(DHAMMERParser::CheckContext *ctx) {
        // Pop the checked term
        AST check_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the check node
        node_stack.push(AST{"CHECK", {std::move(check_body)}});
    }

    void DHAMMERBuilder::exitShow(DHAMMERParser::ShowContext *ctx) {
        std::string def_name = ctx->ID()->getText();

        // Create and push the show node
        node_stack.push(AST{"SHOW", {AST{def_name, {}}}});
    }

    void DHAMMERBuilder::exitShowAll(DHAMMERParser::ShowAllContext *ctx) {
        // Create and push the show all node
        node_stack.push(AST{"SHOWALL", {}});
    }

    void DHAMMERBuilder::exitNormalize(DHAMMERParser::NormalizeContext *ctx) {
        // Pop the expression node from the stack
        AST normalize_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the normalize node
        node_stack.push(AST{"NORMALIZE", {std::move(normalize_body)}});
    }

    void DHAMMERBuilder::exitNormalizeTraced(DHAMMERParser::NormalizeTracedContext *ctx) {
        // Pop the expression node from the stack
        AST normalize_body = std::move(node_stack.top());
        node_stack.pop();

        // Create and push the normalize node
        node_stack.push(AST{"NORMALIZE", {std::move(normalize_body), AST{"TRACE", {}}}});
    }

    void DHAMMERBuilder::exitCheckEq(DHAMMERParser::CheckEqContext *ctx) {
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

    void DHAMMERBuilder::exitBra(DHAMMERParser::BraContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Bra node
        node_stack.push(AST{"BRA", {std::move(term)}});
    }

    void DHAMMERBuilder::exitKet(DHAMMERParser::KetContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Ket node
        node_stack.push(AST{"KET", {std::move(term)}});
    }


    void DHAMMERBuilder::exitDelta(DHAMMERParser::DeltaContext *ctx) {
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
    void DHAMMERBuilder::exitPair(DHAMMERParser::PairContext *ctx) {
        // get e2
        AST e2 = std::move(node_stack.top());
        node_stack.pop();

        // get e1
        AST e1 = std::move(node_stack.top());
        node_stack.pop();
        
        // push Pair node
        node_stack.push(AST{"PAIR", {std::move(e1), std::move(e2)}});
    }


    void DHAMMERBuilder::exitAdj(DHAMMERParser::AdjContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Adj node
        node_stack.push(AST{"ADJ", {std::move(term)}});
    }

    void DHAMMERBuilder::exitConj(DHAMMERParser::ConjContext *ctx) {
        // get term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Conj node
        node_stack.push(AST{"Conjugate", {std::move(term)}});
    }

    void DHAMMERBuilder::exitScr(DHAMMERParser::ScrContext *ctx) {
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
    void DHAMMERBuilder::exitCompo(DHAMMERParser::CompoContext *ctx) {
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
    void DHAMMERBuilder::exitStar(DHAMMERParser::StarContext *ctx) {
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
    void DHAMMERBuilder::exitAdd(DHAMMERParser::AddContext *ctx) {
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
    void DHAMMERBuilder::exitArrow(DHAMMERParser::ArrowContext *ctx) {
        // get T2
        AST t2 = std::move(node_stack.top());
        node_stack.pop();

        // get T1
        AST t1 = std::move(node_stack.top());
        node_stack.pop();
        
        // push ARROW node
        node_stack.push(AST{"ARROW", {std::move(t1), std::move(t2)}});
    }

        void DHAMMERBuilder::exitSum(DHAMMERParser::SumContext *ctx) {
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

    void DHAMMERBuilder::exitIdx(DHAMMERParser::IdxContext *ctx) {
        // get the body
        AST body = std::move(node_stack.top());
        node_stack.pop();

        // get the name
        std::string name = ctx->ID()->getText();

        // push Idx node
        node_stack.push(AST{"IDX", {AST(name, {}), std::move(body)}});
    }

    void DHAMMERBuilder::exitFun(DHAMMERParser::FunContext *ctx) {
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


    void DHAMMERBuilder::exitForall(DHAMMERParser::ForallContext *ctx) {
        // get the body
        AST body = std::move(node_stack.top());
        node_stack.pop();

        // get the name
        std::string name = ctx->ID()->getText();

        // push FORALL node
        node_stack.push(AST{"FORALL", {AST(name, {}), std::move(body)}});
    }

    void DHAMMERBuilder::exitZeroK(DHAMMERParser::ZeroKContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroK node
        node_stack.push(AST{"ZEROK", {std::move(type)}});
    }

    void DHAMMERBuilder::exitZeroB(DHAMMERParser::ZeroBContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroB node
        node_stack.push(AST{"ZEROB", {std::move(type)}});
    }

    void DHAMMERBuilder::exitZeroO(DHAMMERParser::ZeroOContext *ctx) {
        // get the type
        AST type2 = std::move(node_stack.top());
        node_stack.pop();
        AST type1 = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroO node
        node_stack.push(AST{"ZEROO", {std::move(type1), std::move(type2)}});
    }

    void DHAMMERBuilder::exitZeroD(DHAMMERParser::ZeroDContext *ctx) {
        // get the register
        AST reg2 = std::move(node_stack.top());
        node_stack.pop();
        AST reg1 = std::move(node_stack.top());
        node_stack.pop();

        // push ZeroD node
        node_stack.push(AST{"ZEROD", {std::move(reg1), std::move(reg2)}});
    }

    void DHAMMERBuilder::exitOneO(DHAMMERParser::OneOContext *ctx) {
        // get the type
        AST type = std::move(node_stack.top());
        node_stack.pop();

        // push OneO node
        node_stack.push(AST{"ONEO", {std::move(type)}});
    }

    void DHAMMERBuilder::exitBasis0(DHAMMERParser::Basis0Context *ctx) {
        node_stack.push(AST{"BASIS0", {}});
    }

    void DHAMMERBuilder::exitBasis1(DHAMMERParser::Basis1Context *ctx) {
        node_stack.push(AST{"BASIS1", {}});
    }

    void DHAMMERBuilder::exitSubscript1(DHAMMERParser::Subscript1Context *ctx) {
        // get the register
        AST reg = std::move(node_stack.top());
        node_stack.pop();

        // get the term
        AST term = std::move(node_stack.top());
        node_stack.pop();

        // push Subscript1 node
        node_stack.push(AST{"SUBS", {std::move(term), std::move(reg)}});
    }

    void DHAMMERBuilder::exitSubscript2(DHAMMERParser::Subscript2Context *ctx) {
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
    void DHAMMERBuilder::exitRSet(DHAMMERParser::RSetContext *ctx) {
        // 2. Collect all IDs in reverse order
        vector<AST> ids;
        for (int i = 0; i < ctx->ID().size(); ++i) {
            ids.push_back(AST(ctx->ID(i)->getText()));
        }

        // 3. Create and push the RSet node
        node_stack.push(AST{"RSET", std::move(ids)});

    }


    void DHAMMERBuilder::exitEmptyRSet(DHAMMERParser::EmptyRSetContext *ctx) {
        node_stack.push(AST{"RSET"});
    }
        

    void DHAMMERBuilder::exitEmptyApplication(DHAMMERParser::EmptyApplicationContext *ctx) {
        node_stack.push(AST{ctx->ID()->getText(), {}});
    }

    // Handle Application node
    void DHAMMERBuilder::exitApplication(DHAMMERParser::ApplicationContext *ctx) {
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

    void DHAMMERBuilder::exitParen(DHAMMERParser::ParenContext *ctx) {
        // pass
    }

    // Handle Identifier node
    void DHAMMERBuilder::exitIdentifier(DHAMMERParser::IdentifierContext *ctx) {
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
        DHAMMERLexer lexer(&input);
        CommonTokenStream tokens(&lexer);

        tokens.fill();

        // Print the token stream
        if (debug) printTokenStream(tokens);

        DHAMMERParser parser(&tokens);

        // Enable parser trace if debug is true
        if (debug) parser.setTrace(true);

        tree::ParseTree *tree = parser.expr(); // Start from 'expr' rule

        // Create the AST builder
        DHAMMERBuilder treeBuilder;

        // CHECK for syntax errors
        if (parser.getNumberOfSyntaxErrors() == 0) {
            antlr4::tree::ParseTreeWalker::DEFAULT.walk(&treeBuilder, tree);

            // Retrieve the root of the AST
            return treeBuilder.get_root();
        } else {
            return std::nullopt;
        }
    }

} // namespace dhammer
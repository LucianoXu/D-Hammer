#include "astparser.hpp"

namespace astparser {


    class ASTTermBuilder : public ASTBaseListener {
        
    public:
        ASTTermBuilder();

        AST get_root();

        // Called when entering a 'Identifier' node
        void exitIdentifier(ASTParser::IdentifierContext *ctx) override;

        // Called when entering a 'Application' node
        void exitApplication(ASTParser::ApplicationContext *ctx) override;

    private:
        std::stack<AST> node_stack;
    };
    

    ASTTermBuilder::ASTTermBuilder() {}

    AST ASTTermBuilder::get_root() {
        if (!node_stack.empty()) {
            return std::move(node_stack.top());
        }
        
        throw std::runtime_error("No root node found.");
    }

    // Called when entering a 'Identifier' node
    void ASTTermBuilder::exitIdentifier(ASTParser::IdentifierContext *ctx) {
        std::string var_name = ctx->getText();
        node_stack.push(AST{var_name, {}});
    }

    // Called when entering a 'Top' node
    void ASTTermBuilder::exitApplication(ASTParser::ApplicationContext *ctx) {

        // 1. Get the function identifier (first child of the context)
        std::string function_name = ctx->ID()->getText();

        // 2. Initialize a vector to hold argument terms
        std::vector<AST> arguments;

        // 3. Iterate through each argument expression in reverse order
        // (since they were pushed to the stack in reverse)
        for (int i = 0; i < ctx->expr().size(); ++i) {
            // Pop each expression term from the stack and store it in arguments
            arguments.insert(arguments.begin(), node_stack.top());
            node_stack.pop();
        }

        // 4. Create a new term for the application using the function name and arguments
        AST application_term = AST{function_name, std::move(arguments)};

        // 5. Push the constructed application term back onto the stack
        node_stack.push(application_term);
    }


    AST parse(const std::string& code) {
        using namespace antlr4;
        
        ANTLRInputStream input(code);
        ASTLexer lexer(&input);
        CommonTokenStream tokens(&lexer);

        tokens.fill();
        
        ASTParser parser(&tokens);
        tree::ParseTree *tree = parser.expr();

        // Create the tree builder
        ASTTermBuilder treeBuilder;
        antlr4::tree::ParseTreeWalker::DEFAULT.walk(&treeBuilder, tree);

        // Retrieve the root of the custom tree
        return treeBuilder.get_root();
    }

} // namespace ualg
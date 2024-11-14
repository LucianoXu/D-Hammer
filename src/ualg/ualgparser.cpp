#include "ualg.hpp"

namespace ualg{

    UALGTermBuilder::UALGTermBuilder(TermBank& bank) : 
        bank(bank) {}

    const Term* UALGTermBuilder::get_root() {
        if (!node_stack.empty()) {
            return std::move(node_stack.top());
        }
        return nullptr;
    }

    // Called when entering a 'Identifier' node
    void UALGTermBuilder::exitIdentifier(UALGParser::IdentifierContext *ctx) {
        std::string var_name = ctx->getText();
        node_stack.push(bank.get_normal_term(var_name, {}));
    }

    // Called when entering a 'Top' node
    void UALGTermBuilder::exitApplication(UALGParser::ApplicationContext *ctx) {

        // 1. Get the function identifier (first child of the context)
        std::string function_name = ctx->ID()->getText();

        // 2. Initialize a vector to hold argument terms
        std::vector<const Term*> arguments;

        // 3. Iterate through each argument expression in reverse order
        // (since they were pushed to the stack in reverse)
        for (int i = 0; i < ctx->expr().size(); ++i) {
            // Pop each expression term from the stack and store it in arguments
            arguments.insert(arguments.begin(), node_stack.top());
            node_stack.pop();
        }

        // 4. Create a new term for the application using the function name and arguments
        const Term* application_term = bank.get_normal_term(function_name, arguments);

        // 5. Push the constructed application term back onto the stack
        node_stack.push(application_term);
    }


    // parse the input code and build the UALG term tree
    const Term* parse(TermBank& bank, const std::string& code) {
        using namespace antlr4;
        
        ANTLRInputStream input(code);
        UALGLexer lexer(&input);
        CommonTokenStream tokens(&lexer);

        tokens.fill();
        
        UALGParser parser(&tokens);
        tree::ParseTree *tree = parser.expr();

        // Create the tree builder
        UALGTermBuilder treeBuilder(bank);
        antlr4::tree::ParseTreeWalker::DEFAULT.walk(&treeBuilder, tree);

        // Retrieve the root of the custom tree
        return treeBuilder.get_root();
    }

} // namespace ualg
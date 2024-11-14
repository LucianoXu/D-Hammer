#pragma once

#include "antlr4-runtime.h"

#include "UALGLexer.h"
#include "UALGParser.h"
#include "UALGBaseListener.h"

#include "term.hpp"
#include <stack>

namespace ualg{
    
    class UALGTermBuilder : public UALGBaseListener {
    private:
        TermBank& bank;

    public:
        UALGTermBuilder(TermBank& bank);

        const Term* get_root();

        // Called when entering a 'Identifier' node
        void exitIdentifier(UALGParser::IdentifierContext *ctx) override;

        // Called when entering a 'Application' node
        void exitApplication(UALGParser::ApplicationContext *ctx) override;

    private:
        std::stack<const Term *> node_stack;
    };

    const Term* parse(TermBank& bank, const std::string& code);

} // namespace ualg
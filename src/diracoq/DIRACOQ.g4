grammar DIRACOQ;

cmd :   'Def' ID ':=' expr '.'             # Define
    |   'Def' ID ':=' expr ':' expr '.'    # DefineWithType
    |   'Var' ID ':' expr '.'              # Assume
    |   'Check' expr '.'                   # Check
    ;

// Parser rules
expr:   ID                         # Identifier
    |   ID '(' expr (expr)* ')'    # Application
    |   '(' expr ')'               # Parentheses
    ;

// Lexer rules
ID  :   [A-Za-z_][A-Za-z0-9_]*;  // Start with a letter or '_', followed by letters, digits, or '_'
WS  :   [ \t\r\n]+ -> skip ;  // Skip whitespace (spaces, tabs, newlines)

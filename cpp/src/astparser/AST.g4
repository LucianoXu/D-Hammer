grammar AST;

// Parser rules
expr:   ID                         # Identifier
    |   ID '[' expr (',' expr)* ']'    # Application
    |   ID '[' ']'                 # EmptyApplication
    ;

// Lexer rules
ID  :   ~[()[\], \t\r\n]+ ;       // Any character except '(', ')', '[', ']', ',', and whitespace
WS  :   [ \t\r\n]+ -> skip ;  // Skip whitespace (spaces, tabs, newlines)

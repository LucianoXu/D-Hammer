grammar DIRACOQ;


// Lexer rule for multi-line comments
COMMENT: '(*' .*? '*)' -> skip;

expr:   (cmd)+                        # CmdSeq
    |   term                       # FromTerm
    ;

// Parser rules
cmd :   'Def' ID ':=' expr '.'         # Definition0
    |   'Def' ID ':=' expr ':' expr '.'   # Definition1
    |   'Var' ID ':' expr '.'         # Assum
    |   'Check' term '.'               # Check
    |   'Show' ID '.'               # Show
    |   'ShowAll' '.'               # ShowAll
    |   'Normalize' expr '.'         # Normalize
    |   'Normalize' expr 'with' 'trace' '.'         # NormalizeTraced
    |   'CheckEq' expr 'with' expr '.'      # CheckEq
    ;

term:   ID '[' expr (',' expr)* ']'                # Application

    |   '<' term '|'                           # Bra
    |   '|' term '>'                           # Ket
    |   'delta' '(' term ',' term ')'          # Delta
    |   '(' term ',' term ')'                  # Pair
    |   term '^D'                              # Adj
    |   term '^*'                              # Conj
    |   term '.' term                          # Scr
    |   <assoc=left> term term                 # Compo
    |   <assoc=left> term '*' term             # Star
    |   <assoc=left> term '+' term             # Add
    |   <assoc=right> term '->' term           # Arrow
    |   'Sum' ID 'in 'term ',' term   # Sum
    |   'idx' ID '=>' term                     # Idx
    |   'fun' ID ':' term '=>' term            # Fun
    |   'forall' ID '.' term                   # Forall

    |   '0K' '[' term ']'                      # ZeroK
    |   '0B' '[' term ']'                      # ZeroB
    |   '0O' '[' term ',' term ']'             # ZeroO
    |   '1O' '[' term ']'                      # OneO

    |   '(' term ')'                           # Paren
    |   ID                                     # Identifier
    ;

// Lexer rules
ID  :   ([a-zA-Z0-9_$][a-zA-Z0-9_]*)|([+-]?[0-9]+) ;       // standard identifier
WS  :   [ \t\r\n]+ -> skip ;  // Skip whitespace (spaces, tabs, newlines)
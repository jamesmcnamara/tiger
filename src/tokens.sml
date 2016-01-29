structure Tokens =
struct
    type linenum = int
    datatype token =
        TYPE of int * int |
        VAR of int * int |
        FUNCTION of int * int |
        BREAK of int * int |
        OF of int * int |
        END of int * int |
        IN of int * int |
        NIL of int * int |
        LET of int * int |
        DO of int * int |
        TO of int * int |
        FOR of int * int |
        WHILE of int * int |
        ELSE of int * int |
        THEN of int * int |
        IF of int * int |
        ARRAY of int * int |
        ASSIGN of int * int |
        OR of int * int |
        AND of int * int |
        GE of int * int |
        GT of int * int |
        LE of int * int |
        LT of int * int |
        NEQ of int * int |
        EQ of int * int |
        DIVIDE of int * int |
        TIMES of int * int |
        MINUS of int * int |
        PLUS of int * int |
        DOT of int * int |
        RBRACE of int * int |
        LBRACE of int * int |
        RBRACK of int * int |
        LBRACK of int * int |
        RPAREN of int * int |
        LPAREN of int * int |
        SEMICOLON of int * int |
        COLON of int * int |
        COMMA of int * int |
        STRING of string * int * int |
        INT of int * int * int |
        ID of string * int * int |
        EOF;

    fun toString token =
        case token of
            TYPE(i,j) => "TYPE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | VAR(i,j) => "VAR(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | FUNCTION(i,j) => "FUNCTION(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | BREAK(i,j) => "BREAK(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | OF(i,j) => "OF(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | END(i,j) => "END(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | IN(i,j) => "IN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | NIL(i,j) => "NIL(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | LET(i,j) => "LET(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | DO(i,j) => "DO(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | TO(i,j) => "TO(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | FOR(i,j) => "FOR(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | WHILE(i,j) => "WHILE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | ELSE(i,j) => "ELSE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | THEN(i,j) => "THEN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | IF(i,j) => "IF(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | ARRAY(i,j) => "ARRAY(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | ASSIGN(i,j) => "ASSIGN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | OR(i,j) => "OR(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | AND(i,j) => "AND(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | GE(i,j) => "GE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | GT(i,j) => "GT(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | LE(i,j) => "LE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | LT(i,j) => "LT(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | NEQ(i,j) => "NEQ(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | EQ(i,j) => "EQ(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | DIVIDE(i,j) => "DIVIDE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | TIMES(i,j) => "TIMES(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | MINUS(i,j) => "MINUS(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | PLUS(i,j) => "PLUS(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | DOT(i,j) => "DOT(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | RBRACE(i,j) => "RBRACE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | LBRACE(i,j) => "LBRACE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | RBRACK(i,j) => "RBRACK(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | LBRACK(i,j) => "LBRACK(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | RPAREN(i,j) => "RPAREN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | LPAREN(i,j) => "LPAREN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | SEMICOLON(i,j) => "SEMICOLON(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | COLON(i,j) => "COLON(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | COMMA(i,j) => "COMMA(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | STRING(s,i,j) => "STRING(" ^ s ^ "," ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | INT(n,i,j) => "INT(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | ID(s,i,j) => "ID(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ ")"
          | EOF => "EOF";
end

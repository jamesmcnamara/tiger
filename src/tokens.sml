structure Tokens =
struct
    type linenum = int
    datatype token =
        TYPE of int * int * int |
        VAR of int * int * int |
        FUNCTION of int * int * int |
        BREAK of int * int * int |
        OF of int * int * int |
        END of int * int * int |
        IN of int * int * int |
        NIL of int * int * int |
        LET of int * int * int |
        DO of int * int * int |
        TO of int * int * int |
        FOR of int * int * int |
        WHILE of int * int * int |
        ELSE of int * int * int |
        THEN of int * int * int |
        IF of int * int * int |
        ARRAY of int * int * int |
        ASSIGN of int * int * int |
        OR of int * int * int |
        AND of int * int * int |
        GE of int * int * int |
        GT of int * int * int |
        LE of int * int * int |
        LT of int * int * int |
        NEQ of int * int * int |
        EQ of int * int * int |
        DIVIDE of int * int * int |
        TIMES of int * int * int |
        MINUS of int * int * int |
        PLUS of int * int * int |
        DOT of int * int * int |
        RBRACE of int * int * int |
        LBRACE of int * int * int |
        RBRACK of int * int * int |
        LBRACK of int * int * int |
        RPAREN of int * int * int |
        LPAREN of int * int * int |
        SEMICOLON of int * int * int |
        COLON of int * int * int |
        COMMA of int * int * int |
        STRING of string * int * int * int |
        INT of int * int * int * int |
        ID of string * int * int * int |
        EOF of int * int * int;

    fun toString token =
        case token of
            TYPE(i,j,l) => "TYPE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            VAR(i,j,l) => "VAR(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            FUNCTION(i,j,l) => "FUNCTION(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            BREAK(i,j,l) => "BREAK(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            OF(i,j,l) => "OF(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            END(i,j,l) => "END(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            IN(i,j,l) => "IN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            NIL(i,j,l) => "NIL(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            LET(i,j,l) => "LET(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            DO(i,j,l) => "DO(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            TO(i,j,l) => "TO(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            FOR(i,j,l) => "FOR(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            WHILE(i,j,l) => "WHILE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            ELSE(i,j,l) => "ELSE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            THEN(i,j,l) => "THEN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            IF(i,j,l) => "IF(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            ARRAY(i,j,l) => "ARRAY(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            ASSIGN(i,j,l) => "ASSIGN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            OR(i,j,l) => "OR(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            AND(i,j,l) => "AND(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            GE(i,j,l) => "GE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            GT(i,j,l) => "GT(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            LE(i,j,l) => "LE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            LT(i,j,l) => "LT(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            NEQ(i,j,l) => "NEQ(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            EQ(i,j,l) => "EQ(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            DIVIDE(i,j,l) => "DIVIDE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            TIMES(i,j,l) => "TIMES(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            MINUS(i,j,l) => "MINUS(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            PLUS(i,j,l) => "PLUS(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            DOT(i,j,l) => "DOT(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            RBRACE(i,j,l) => "RBRACE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            LBRACE(i,j,l) => "LBRACE(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            RBRACK(i,j,l) => "RBRACK(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            LBRACK(i,j,l) => "LBRACK(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            RPAREN(i,j,l) => "RPAREN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            LPAREN(i,j,l) => "LPAREN(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            SEMICOLON(i,j,l) => "SEMICOLON(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            COLON(i,j,l) => "COLON(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            COMMA(i,j,l) => "COMMA(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            STRING(s,i,j,l) => "STRING(" ^ s ^ "," ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            INT(n,i,j,l) => "INT(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            ID(s,i,j,l) => "ID(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")" |
            EOF(i,j,l) => "EOF(" ^ Int.toString(i) ^ "," ^ Int.toString(j) ^ "," ^ Int.toString(l) ^ ")";
end

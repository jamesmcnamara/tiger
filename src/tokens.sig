(* TODO: Use signatures? *)
signature Tiger_TOKENS =
sig
    type linenum
    type token

    val TYPE: int * int * int -> token
    val VAR: int * int * int -> token
    val FUNCTION: int * int * int -> token
    val BREAK: int * int * int -> token
    val OF: int * int * int -> token
    val END: int * int * int -> token
    val IN: int * int * int -> token
    val NIL: int * int * int -> token
    val LET: int * int * int -> token
    val DO: int * int * int -> token
    val TO: int * int * int -> token
    val FOR: int * int * int -> token
    val WHILE: int * int * int -> token
    val ELSE: int * int * int -> token
    val THEN: int * int * int -> token
    val IF: int * int * int -> token
    val ARRAY: int * int * int -> token
    val ASSIGN: int * int * int -> token
    val OR: int * int * int -> token
    val AND: int * int * int -> token
    val GE: int * int * int -> token
    val GT: int * int * int -> token
    val LE: int * int * int -> token
    val LT: int * int * int -> token
    val NEQ: int * int * int -> token
    val EQ: int * int * int -> token
    val DIVIDE: int * int * int -> token
    val TIMES: int * int * int -> token
    val MINUS: int * int * int -> token
    val PLUS: int * int * int -> token
    val DOT: int * int * int -> token
    val RBRACE: int * int * int -> token
    val LBRACE: int * int * int -> token
    val RBRACK: int * int * int -> token
    val LBRACK: int * int * int -> token
    val RPAREN: int * int * int -> token
    val LPAREN: int * int * int -> token
    val SEMICOLON: int * int * int -> token
    val COLON: int * int * int -> token
    val COMMA: int * int * int -> token
    val STRING: string * int * int * int -> token
    val INT: int * int * int * int -> token
    val ID: string * int * int * int -> token
    val EOF: int * int * int -> token
end

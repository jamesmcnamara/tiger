(* TODO: Use signatures? *)
signature Tiger_TOKENS =
sig
    type linenum
    type token

    val TYPE: int * int -> token
    val VAR: int * int -> token
    val FUNCTION: int * int -> token
    val BREAK: int * int -> token
    val OF: int * int -> token
    val END: int * int -> token
    val IN: int * int -> token
    val NIL: int * int -> token
    val LET: int * int -> token
    val DO: int * int -> token
    val TO: int * int -> token
    val FOR: int * int -> token
    val WHILE: int * int -> token
    val ELSE: int * int -> token
    val THEN: int * int -> token
    val IF: int * int -> token
    val ARRAY: int * int -> token
    val ASSIGN: int * int -> token
    val OR: int * int -> token
    val AND: int * int -> token
    val GE: int * int -> token
    val GT: int * int -> token
    val LE: int * int -> token
    val LT: int * int -> token
    val NEQ: int * int -> token
    val EQ: int * int -> token
    val DIVIDE: int * int -> token
    val TIMES: int * int -> token
    val MINUS: int * int -> token
    val PLUS: int * int -> token
    val DOT: int * int -> token
    val RBRACE: int * int -> token
    val LBRACE: int * int -> token
    val RBRACK: int * int -> token
    val LBRACK: int * int -> token
    val RPAREN: int * int -> token
    val LPAREN: int * int -> token
    val SEMICOLON: int * int -> token
    val COLON: int * int -> token
    val COMMA: int * int -> token
    val STRING: string * int * int -> token
    val INT: int * int * int -> token
    val ID: string * int * int -> token
    val EOF: unit -> token
end

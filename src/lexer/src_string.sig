signature SRC_STRING =
sig
    type yypos = int
    exception StringNotClosed of yypos
    val reset : unit -> unit
    val new : yypos -> unit
    val pushString : string * yypos -> unit
    val pushAscii : string * yypos -> unit
    val pushControl : string * yypos -> unit
    val emit : yypos -> UserDeclarations.lexresult
    val getStartPos : unit -> yypos
    val closed : unit -> bool
end

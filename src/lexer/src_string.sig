signature SRC_STRING =
sig
    type yypos = int
    exception StringNotClosed of int * int (* line position * line number *)
    val getStartPos : unit -> int
    val isBuildingString : unit -> bool
    val new : yypos -> unit
    val pushString : string * yypos -> unit
    val pushAscii : string * yypos -> unit
    val pushControl : string * yypos -> unit
    val emit : yypos -> Token.token
end

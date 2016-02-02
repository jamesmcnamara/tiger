signature SRC_COMMENT =
sig
    type yypos = int
    exception CommentNotClosed of yypos
    val reset : unit -> unit
    val start : yypos -> unit
    val finish : yypos -> unit
    val getStartPos : unit -> yypos
    val closed : unit -> bool
end

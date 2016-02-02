signature SRC_COMMENT =
sig
    type yypos = int

    exception CommentNotClosed of int * int (* line position * line number *)

    val reset : unit -> unit
    val start : yypos -> unit
    val finish : yypos -> unit
    val closed : unit -> bool
    val getComments : unit -> yypos list
end

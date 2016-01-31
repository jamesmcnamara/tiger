signature SRC_COMMENT =
sig
    type yypos = int

    val reset : unit -> unit
    val start : yypos -> unit
    val finish : yypos -> unit
    val closed : unit -> bool
end

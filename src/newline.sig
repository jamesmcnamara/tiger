signature NEWLINE =
sig
    type yypos = int

    (*
    Resets the internal state for this structure.
    *)
    val reset : unit -> unit
    (*
    Adds a newline at the given yypos.
    *)
    val add : yypos -> unit
    (*
    Returns the line number which the given yypos is in. Newlines
    themselves are considered to be the 0th character of a line.
    *)
    val getLine : yypos -> int
    (*
    Returns the character position relative to the given yypos's line.
    Newlines themselves are considered to be the 0th character of a line.
    *)
    val getPos : yypos -> int
end

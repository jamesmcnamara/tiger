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

structure Newline :> NEWLINE =
struct
    val lines = ref [1]

    type yypos = int

    fun reset () =
        lines := [1]

    fun add yypos =
        lines := yypos :: !lines

    fun getLine yypos =
        let fun look (p::ps, n) = if p <= yypos then n else look(ps, n-1)
            |   look _ = 0
        in
            look(!lines, List.length(!lines))
        end

    fun getPos yypos =
        let fun look (p::ps, n) = if p <= yypos then yypos - p else look(ps, n-1)
            |   look _ = 0
        in
            look(!lines, List.length(!lines))
        end
end

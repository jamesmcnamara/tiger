signature TRACKING =
sig
    (* Resets the internal state for this structure. *)
    val reset : unit -> unit
    (* Adds a newline at the given yypos. *)
    val newline : int -> unit
    (* Returns the line number which the given yypos is in. *)
    val getLine : int -> int
    (* Returns the character position relative to the given yypos's line. *)
    val getPosInLine : int -> int
end

structure Tracking :> TRACKING =
struct
    val lines = ref [1]

    fun reset () =
        lines := [1]

    fun newline yypos =
        lines := yypos :: !lines

    fun getLine yypos =
        1

    fun getPosInLine yypos =
        1
end

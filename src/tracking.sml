signature TRACKING =
sig
    val lineNum : int ref
    val linePos : int list ref
    val currentLineStartPos : int ref
    val reset : unit -> unit
    val incrementLine : int -> unit
    val appendPosition : int -> unit
    val getLinePos : int -> int
end

structure Tracking =
struct
    val lineNum = ref 1
    val linePos = ref [1]
    val currentLineStartPos = ref 1

    fun reset() =
        (lineNum := 1;
         linePos := [1];
         currentLineStartPos := 1)

    fun incrementLine yypos =
        (lineNum := !lineNum + 1;
         linePos := yypos :: !linePos;
         currentLineStartPos := yypos)

    fun appendPosition yypos =
        linePos := yypos :: !linePos

    fun getLinePos yypos = (yypos - !currentLineStartPos)
end

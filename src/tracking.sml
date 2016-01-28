signature TRACKING =
sig
    val reset : unit -> unit
    val shift : int -> unit
    val newline : unit -> unit
    val getPosition : unit -> int
    val getLine : unit -> int
end

structure Tracking :> TRACKING =
struct
    val linePos = ref 1
    val lineNum = ref 1

    fun reset () =
        (linePos := 1;
         lineNum := 1)

    fun shift (amt) =
        linePos := !linePos + amt

    fun newline () =
        (linePos := 1;
         lineNum := !lineNum + 1)

    fun getPosition () =
        !linePos

    fun getLine () =
        !lineNum
end

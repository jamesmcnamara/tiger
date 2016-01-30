signature ERRORMSG =
sig
    exception Error
    val anyErrors : bool ref
    val fileName : string ref
    val sourceStream : TextIO.instream ref
    val error : int -> string -> unit
    val impossible : string -> 'a   (* raises Error *)
    val reset : unit -> unit
end

structure ErrorMsg :> ERRORMSG =
struct
  exception Error

  val anyErrors = ref false
  val fileName = ref ""
  val sourceStream = ref TextIO.stdIn

  fun reset () =
    (anyErrors := false;
		 fileName := "";
		 sourceStream := TextIO.stdIn)

  fun error yypos msg =
    let val line = Int.toString(Newline.getLine(yypos))
        val pos = Int.toString(Newline.getPos(yypos))
    in
      anyErrors := true;
      (app print [!fileName, ":", line, ".", pos, ":", msg, "\n"])
    end

  fun impossible msg =
    (app print ["Error: Compiler bug: ", msg, "\n"];
     TextIO.flushOut TextIO.stdOut;
     raise Error)

end

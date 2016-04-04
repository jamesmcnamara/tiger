signature FRAME =
sig
  type frame
  datatype access = InFrame of int | InReg of Temp.temp
  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val formals: frame -> access list
  val name: frame -> Temp.label
  val offset: frame -> int
  val allocLocal: frame -> bool -> access

  val externalCall: string * Tree.exp list -> Tree.exp
  val FP: Temp.temp
  val wordSize: int

  (* For tests *)
  val registersAsTemps: int list

  (*val exp: access -> Tree.exp -> Tree.exp*)

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

end

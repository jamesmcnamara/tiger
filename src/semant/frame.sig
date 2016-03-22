signature FRAME =
sig
  type frame
  type access
  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val formals: frame -> access list
  val name: frame -> Temp.label
  val allocLocal: frame -> bool -> access

  val externalCall: string * Tree.exp list -> Tree.exp
  (*val FP: Temp.temp
  val wordSize: int
  val exp: access -> Tree.exp -> Tree.exp
  *)
  (* TODO: These are needed *)
  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string
end

signature FRAME =
sig
  type frame
  type register
  datatype access = InFrame of int | InReg of Temp.temp
  val newFrame: {name: Temp.label, formals: bool list} -> frame
  val formals: frame -> access list
  val name: frame -> Temp.label
  val offset: frame -> int
  val allocLocal: frame -> bool -> access

  val externalCall: string * Tree.exp list -> Tree.exp
  val FP: Temp.temp
  val RA: Temp.temp
  val RV: Temp.temp
  val wordSize: int
  val sizeOfK: int
  val registers: register list
  val availableRegisters: register list

  (* For tests *)
  val registersAsTemps: int list

  (*val exp: access -> Tree.exp -> Tree.exp*)

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val tempMap : register Temp.Table.table
end

signature MAIN =
sig
    val compile : string -> unit
end

structure Main : MAIN =
struct
fun compile filename =
  let
    fun toAsm(Frame.PROC {body, frame}) =
        CodeGen.codegen frame body
      | toAsm(Frame.STRING (_, _)) = []

    val exp = Parse.parse(filename)
    val _ =  FindEscape.findEscape(exp)
    val ir = Semant.transProg(exp)
    val asm = List.map toAsm ir
    val out = foldl (fn (a, s) => (foldl (fn (i, s) => Assem.format Temp.makestring i) s a)) "" asm
  in
    print out
  end
end

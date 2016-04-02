signature MAIN =
sig
    (* TODO: Should we return a unit type for now? *)
    val compile : string -> Assem.instr list list
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
    val expy = Semant.transProg(exp)
    val frags = Translate.getResult()
    val asm = List.map toAsm frags
  in
    asm
  end
end

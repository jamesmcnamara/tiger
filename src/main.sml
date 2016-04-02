signature MAIN =
sig
    (* TODO: Should we return a unit type for now? *)
    (* val compile : string -> Assem.instr list list *)
    val compile : string -> MipsFrame.frag list
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
  in
    ir
  end
end

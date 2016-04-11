signature MAIN =
sig
    (* TODO: Should we return a unit type for now? *)
    (* val compile : string -> Assem.instr list list *)
    val compile : string -> (Flow.flowgraph * Flow.Graph.node list) list

    val generateCFG : (Symbol.symbol * int * Assem.instr list) list -> (Liveness.igraph * (Flow.Graph.node -> Temp.temp list)) list
end

structure Main : MAIN =
struct
exception NotImplemented


fun generateCFG([]) = []
  | generateCFG((s,i,a)::rest) = (Liveness.interferenceGraph(MakeGraph.instrs2graph(a)))::generateCFG(rest)

fun compile filename =
  let
    fun toAsm(Frame.PROC {body, frame}) =
        let val assem = CodeGen.codegen frame body
        in
          MakeGraph.instrs2graph(assem)
        end
      | toAsm(Frame.STRING (_, _)) = raise NotImplemented

    val exp = Parse.parse(filename)
    val _ =  FindEscape.findEscape(exp)
    val ir = Semant.transProg(exp)
    val asm = List.map toAsm ir
  in
    asm
  end
end

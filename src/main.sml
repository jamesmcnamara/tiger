signature MAIN =
sig
    val compile : string -> unit

    val generateCFG : (Symbol.symbol * int * Assem.instr list) list -> (Liveness.igraph * (Flow.Graph.node -> Temp.temp list)) list

    val printI : (Liveness.igraph * (Flow.Graph.node -> Temp.temp list)) list -> unit
end

structure Main : MAIN =
struct
exception NotImplemented

fun printI [] = ()
  | printI((graph,_)::rest) = Liveness.show(graph)

fun generateCFG([]) = []
  | generateCFG((s,i,a)::rest) = (Liveness.interferenceGraph(MakeGraph.instrs2graph(a)))::generateCFG(rest)

fun compile filename =
  let
    fun toAsm(Frame.PROC {body, frame}) =
        let val assem = CodeGen.codegen frame body
        in
          RegAlloc.alloc(assem,frame)
        end
      | toAsm(Frame.STRING (label, value)) = ([], Temp.Table.empty) (* temporary hack *)

    val exp = Parse.parse(filename)
    val _ =  FindEscape.findEscape(exp)
    val ir = Semant.transProg(exp)
    val asm = (List.map (#1 o toAsm) ir)
    (*val out = foldr (fn (a, s) => (foldr (fn (i, s) => Assem.format Temp.makestring i ^ s) s a)) "" asm*)
  in
    (* Print the IR fragments. *)
    (map (fn f =>
      (case f of
        MipsFrame.PROC {body, frame} =>
          Printtree.printtree (TextIO.stdOut, body)
      | MipsFrame.STRING (l, s) => print(s ^ "\n")))
      ir);

    print "\n";

    (* Print the Assem. *)
    (*print out*)
    ()
  end
end

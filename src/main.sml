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
    (* TODO: This function should just return a list of strings to print.
     * Once we have an allocator that works we will format the registers
     * with the allocation. *)
    fun toAsm(Frame.PROC {body, frame}) =
        let
          (* Canon *)
          val stms = Canon.linearize body
          val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
          val assem = List.concat(map (CodeGen.codegen frame) stms')

          (* TODO: Use the `assem'` when we have `procEntryExit3` *)
          (*val { prolog, epilog, body=assem'} = Frame.procEntryExit3(frame, assem)*)
          (*val (assem'', alloc) = RegAlloc.alloc(assem', frame)*)
          val (assem'', alloc) = RegAlloc.alloc(assem, frame)
          val replace = (fn(temp) => valOf(Temp.Table.look(alloc,temp)))
          val replaceAll = map (fn(instr) => Assem.format replace instr) assem''
          val replaced = foldr (fn(a,b) => a ^ b) "" replaceAll
        in
          (assem'', alloc, replaced)
        end
      | toAsm(Frame.STRING (label, value)) =
        (* TODO: temporary hack *)
        ([], Temp.Table.empty, "")

    val exp = Parse.parse(filename)
    val _ =  FindEscape.findEscape(exp)
    val ir = Semant.transProg(exp)
    val asm = (List.map (#3 o toAsm) ir)
  in
    (* Print the IR fragments. *)
    print "\nIR\n-----\n";
    (map (fn f =>
      (case f of
        MipsFrame.PROC {body, frame} =>
          Printtree.printtree (TextIO.stdOut, body)
      | MipsFrame.STRING (l, s) => print(s ^ "\n")))
      ir);

    (* Print the Assem. *)
    (*print "\nASM\n-----\n";
    print (foldr (fn (a, s) =>
      (foldr (fn (i, s) => Assem.format Temp.makestring i ^ s) s a)) "" asm);*)

    print "\nFinal\n-----\n";
    print (foldr (fn(a,b) => a ^ b) "" asm)
  end
end

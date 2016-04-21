signature REG_ALLOC =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val alloc : Assem.instr list * Frame.frame -> Assem.instr list * allocation
end

structure RegAlloc : REG_ALLOC =
struct
  structure Frame = MipsFrame
  type allocation = Frame.register Temp.Table.table
  exception CannotAllocateError

  fun alloc (instrs, frame) =
    let val cfg = MakeGraph.instrs2graph(instrs)
        val (interference, liveOut) = Liveness.interferenceGraph(cfg)
        val _ = Liveness.show(interference)
        (* TODO: What to do with the spills? *)
        (*val (alloc', spills) =
          Color.color { interference = interference,
                        initial = Frame.tempMap,
                        spillCost = (fn node => 0),
                        registers = Frame.registers }*)
    in
      (instrs, Frame.tempMap)
    end
end

(*
datatype igraph = IGRAPH of {graph: graph,
                             tnode: Temp.temp -> node,
                             gtemp: node -> Temp.temp,
                             moves: (node * node) list}
*)

(*
  General Algorithm
  1. Create a stack of nodes to color.
     Find the first node which can be removed(where adj < K)
     Remove the node and add it to a stack to color
     Repeat until no nodes are left
  2. Add nodes back to the graph and decide on color
*)

(*
  TODO: This almost completes part 1.
        It doesnt remove the edges from nodes that are on the stack
*)

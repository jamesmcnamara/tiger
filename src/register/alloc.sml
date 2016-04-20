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
        val (igraph, tempsOut) = Liveness.interferenceGraph(cfg)

        fun findNodeToRemove(nodes) =
            let fun findNodeToRemoveAcc([],empty) =
                    if empty then NONE else raise CannotAllocateError
                  | findNodeToRemoveAcc(n::rest,_) =
                    (print(Int.toString(List.length(Graph.adj n)) ^ "\n\n");
                    (if List.length(Graph.adj n) < Frame.sizeOfK
                    then SOME(n) else findNodeToRemoveAcc(rest,false)))
            in
              findNodeToRemoveAcc(nodes,true)
            end

        fun removeNode([],n) = []
          | removeNode(node::nodes,n) =
            if Graph.eq(node,n) then nodes else node::removeNode(nodes,n)

        fun buildStack([],s) = s
          | buildStack(node::nodes,s) =
            case findNodeToRemove(node::nodes) of
                NONE => raise CannotAllocateError
              | SOME(n) =>
                let val reducedNodes = removeNode(node::nodes,n)
                in
                  Stack.push(s,n);
                  buildStack(reducedNodes,s)
                end

        val nodeStack = case igraph of
          Liveness.IGRAPH{graph=g, tnode=t, gtemp=gt, moves=ms} =>
            buildStack(Graph.nodes(g),Stack.empty())
    in
      (Stack.printStack(nodeStack);
       (instrs, Temp.Table.empty))
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

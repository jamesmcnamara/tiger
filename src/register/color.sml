signature COLOR =
sig
  structure Frame : FRAME
  type allocation = Frame.register Temp.Table.table
  val color: {interference: Liveness.igraph,
              initial: allocation,
              spillCost: Graph.node -> int,
              registers: Frame.register list}
              -> allocation * Temp.temp list
end

structure Color :> COLOR =
struct
  structure Frame : FRAME = MipsFrame
  type allocation = Frame.register Temp.Table.table

  (* TODO: Use these to implement Color.color. *)
  (*fun findNodeToRemove(nodes) =
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
      buildStack(Graph.nodes(g),Stack.empty())*)

  (* TODO: Write this. *)
  fun color { interference,
              initial,
              spillCost,
              registers } =
  (initial, [])

end

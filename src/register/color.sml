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

structure Color : COLOR =
struct
  structure Frame : FRAME = MipsFrame
  type allocation = Frame.register Temp.Table.table

  exception CannotAllocateError
  exception NotEnoughRegisters

  structure RegisterSet = BinarySetFn(struct type ord_key = Frame.register
                                     val compare = String.compare
                              end)

  fun color { interference,
              initial,
              spillCost,
              registers } =
    let fun findNodeToRemove(nodes) =
          let fun findNodeToRemoveAcc([],empty) =
                  if empty then NONE else raise CannotAllocateError
                | findNodeToRemoveAcc(n::rest,_) =
                  (if List.length(Graph.adj n) < Frame.sizeOfK
                   then SOME(n) else findNodeToRemoveAcc(rest,false))
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

        val nodeStack = case interference of
          Liveness.IGRAPH{graph=g, tnode=t, gtemp=gt, moves=ms} =>
            buildStack(Graph.nodes(g),Stack.empty())

        fun enumerateAdjacentColors(adjacentNodes,alloc,gtemp) =
          let fun buildSet([],accSet) = accSet
                | buildSet(node::nodes,accSet) =
                  case Temp.Table.look(alloc,gtemp(node)) of
                      NONE => buildSet(nodes,accSet)
                    | SOME(r) => buildSet(nodes,RegisterSet.add(accSet,r))
          in
            buildSet(adjacentNodes,RegisterSet.empty)
          end

          (* return a set of colors*)

        (* Get all adjacent nodes
           Enumerate the assigned colors from each adjacent node
           Take the difference of the available colors and the used colors
           If there is an unused color, assign it, otherwise raise NotEnoughRegisters
        *)
        (* Check each adjacent node. Find the lowest available register *)
        fun lowestAvailableRegister(node,alloc,gt) =
          let val adj = Graph.adj(node)
              val adjacentColors = enumerateAdjacentColors(adj,alloc,gt)
              val allColors = RegisterSet.addList(RegisterSet.empty, Frame.availableRegisters)
              val availableColors = RegisterSet.difference(allColors, adjacentColors)
          in
            if RegisterSet.numItems(availableColors) > 0 then hd(RegisterSet.listItems(availableColors))
            else raise NotEnoughRegisters
          end

        fun apply (stack,alloc,gt) =
          case Stack.pop(stack) of
              NONE => alloc
            | SOME(n) =>
              let val register = lowestAvailableRegister(n,alloc,gt)
                  val updatedAllocation = Temp.Table.enter(alloc,gt(n),register)
              in
                apply(stack,updatedAllocation,gt)
              end

  in
    case interference of
      Liveness.IGRAPH{graph=g, tnode=t, gtemp=gt, moves=ms} =>
      (apply(nodeStack,initial,gt), [])
  end

end

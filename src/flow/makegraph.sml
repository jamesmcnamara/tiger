signature MAKEGRAPH =
sig
  (*val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list*)
  val instrs2graph: Assem.instr list -> Flow.flowgraph * Flow.Graph.node list
end

structure MakeGraph : MAKEGRAPH =
struct

  structure G = Graph
  structure A = Assem

  exception BadLabel
  exception ArityError

  fun instrs2graph instrs =
    let val graph = G.newGraph()
        fun labelNode(A.LABEL{assem,lab}) = (SOME(lab), (G.newNode graph))
          | labelNode _ = (NONE, G.newNode graph)

        val nodeLabel = map labelNode instrs
        val nodes = map (fn (_,node) => node) nodeLabel

        fun getNode(label,[]) = raise BadLabel
          | getNode(label,(SOME(lab),node)::r) =
            if label = lab then node else getNode(label,r)
          | getNode(label,f::r) = getNode(label,r)

        fun makeEdgeForJump(instr,node) =
          case instr of
              A.OPER{assem=assem,dst=dst,src=src,jump=SOME(jumps)} =>
                app (fn (label) => G.mk_edge {from=node,to=getNode(label,nodeLabel)}) jumps
            | _ => ()

        (* -takes in the previous (instruction,node) and the list of remaining instructions/nodes
           -if there is a next instruction, it makes an edge between the prev and next
           -if the previous instruction has a jump, this also creates an edge for it
        *)
        fun makeEdges(prev,[],[]) = makeEdgeForJump(prev)
          | makeEdges((pInstr,pNode), instr::instrs, node::nodes) =
            (makeEdgeForJump(pInstr,pNode);
             G.mk_edge {from=pNode,to=node};
             makeEdges((instr,node),instrs,nodes))
          | makeEdges _ = raise ArityError

        fun setEdges([],[]) = ()
          | setEdges(instr::instrs,node::nodes) = makeEdges((instr,node), instrs, nodes)
          | setEdges _ = raise ArityError

        val defTable = Graph.Table.empty
        val useTable = Graph.Table.empty
        val isMoveTable = Graph.Table.empty

        fun updateCFG(instr,node) =
          case instr of
              A.LABEL{assem,lab} =>
                (Graph.Table.enter(defTable,node,[]);
                 Graph.Table.enter(useTable,node,[]);
                 Graph.Table.enter(isMoveTable,node,false);
                 ())
            | A.OPER{assem,dst,src,jump} =>
              (Graph.Table.enter(defTable,node,dst);
               Graph.Table.enter(useTable,node,src);
               Graph.Table.enter(isMoveTable,node,false);
               ())
            | A.MOVE{assem,dst,src} =>
              (Graph.Table.enter(defTable,node,[dst]);
               Graph.Table.enter(useTable,node,[src]);
               Graph.Table.enter(isMoveTable,node,true);
               ())

        val instrsNodes = ListPair.map (fn (instr,node) => (instr,node)) (instrs, nodes)

    in
      (setEdges(instrs,nodes);
       app (fn pair => updateCFG pair) instrsNodes;
       (Flow.FGRAPH{control=graph,def=defTable,use=useTable,ismove=isMoveTable},nodes))
    end
end


(*
  Step 1 -> need to associate a label with nodes
  Step 2 -> make edges according to assembly
         -> for each jump, find the node that has the corresponding label and make an edge
         -> otherwise just make an edge connecting the next instruction


*)

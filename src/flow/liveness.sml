signature LIVENESS =
sig
  type graph = Graph.graph
  type node = Graph.node
  eqtype temp
  structure Set : ORD_SET sharing type Set.Key.ord_key = temp
  type liveSet = Set.set
  type liveMap = liveSet Flow.Graph.Table.table
  datatype igraph = IGRAPH of {graph: graph,
                               tnode: Temp.temp -> node,
                               gtemp: node -> Temp.temp,
                               moves: (node * node) list}

  val interferenceGraph: Flow.flowgraph * Flow.Graph.node list -> liveMap * liveMap
  (*val interferenceGraph: Flow.flowgraph -> igraph * (Flow.Graph.node -> Temp.temp list)*)

  (*val show: TextIO.outstream * igraph -> unit*)

end

structure Liveness : LIVENESS =
struct
  type graph = Graph.graph
  type node = Graph.node
  type temp = Temp.temp

  structure Set = BinarySetFn(struct type ord_key = Temp.temp
                                     val compare = Int.compare
                              end)
  exception Something

  type liveSet = Set.set
  type liveMap = liveSet Flow.Graph.Table.table

  datatype igraph = IGRAPH of {graph: graph,
                               tnode: Temp.temp -> node,
                               gtemp: node -> Temp.temp,
                               moves: (node * node) list}

  fun interferenceGraph(Flow.FGRAPH{control,def,use,ismove},_) =
    let val emptyLiveSet = Set.empty
        val emptyLiveMap: liveMap = Flow.Graph.Table.empty

        fun setEmptyLiveness([],liveInMap,liveOutMap) = (liveInMap,liveOutMap)
          | setEmptyLiveness(node::nodes,liveInMap,liveOutMap) =
             setEmptyLiveness(nodes,
                              Flow.Graph.Table.enter(liveInMap,node,emptyLiveSet),
                              Flow.Graph.Table.enter(liveOutMap,node,emptyLiveSet))

        val (liveInMap:liveMap,liveOutMap:liveMap) = setEmptyLiveness(Graph.nodes(control),emptyLiveMap,emptyLiveMap)

        fun toSet(NONE) = Set.empty
          | toSet(SOME(l)) = Set.addList(Set.empty,l)
        fun unpackSet(NONE)  = Set.empty
          | unpackSet(SOME(s)) = s

        fun setLiveness(node,(liveInMap,liveOutMap)) =
          let val useS = toSet(Graph.Table.look(use,node))
              val outN = unpackSet(Flow.Graph.Table.look(liveOutMap,node))
              val defN = toSet(Graph.Table.look(def,node))
              val outMinusDefS = Set.difference(outN, defN)
              val inSet = Set.union(useS, outMinusDefS)
              val outSet = foldl (fn (n,s) => Set.union(unpackSet(Flow.Graph.Table.look(liveInMap,n)),s)) Set.empty (Graph.succ(node))
          in
            (Flow.Graph.Table.enter(liveInMap,node,inSet),
             Flow.Graph.Table.enter(liveInMap,node,outSet))
          end

        fun computeLiveness(liveIn,liveOut) =
          let val (liveIn',liveOut') = foldl (fn (node,liveness) => setLiveness(node,liveness))
                                             (liveIn,liveOut)
                                             (Graph.nodes(control))
          in
            (liveIn',liveOut')
            (*if liveIn = liveIn' andalso liveOut = liveOut'
            then (liveIn,liveOut)
            else computeLiveness(liveIn',liveOut')*)
            (* TODO: we need to check equality of the new liveIn/liveOut with the old ones *)
          end

    in

      (liveInMap,liveOutMap)
    end

end

signature LIVENESS =
sig
  type graph = Graph.graph
  type node = Graph.node
  eqtype temp
  structure TempSet : ORD_SET sharing type TempSet.Key.ord_key = temp
  type liveSet = TempSet.set * node list
  type liveMap = liveSet Flow.Graph.Table.table
  datatype igraph = IGRAPH of {graph: graph,
                               tnode: Temp.temp -> node,
                               gtemp: node -> Temp.temp,
                               moves: (node * node) list}

  val interferenceGraph: Flow.flowgraph * Flow.Graph.node list -> igraph * (Flow.Graph.node -> Temp.temp list)
  exception NodeDoesNotExist
  (*val show: TextIO.outstream * igraph -> unit*)

end

structure Liveness : LIVENESS =
struct
  type graph = Graph.graph
  type node = Graph.node
  type temp = Temp.temp

  structure TempSet = BinarySetFn(struct type ord_key = Temp.temp
                                     val compare = Int.compare
                              end)

  exception NodeDoesNotExist

  type liveSet = TempSet.set * node list
  type liveMap = liveSet Flow.Graph.Table.table

  datatype igraph = IGRAPH of {graph: graph,
                               tnode: Temp.temp -> node,
                               gtemp: node -> Temp.temp,
                               moves: (node * node) list}

  fun interferenceGraph(Flow.FGRAPH{control,def,use,ismove},_) =
    let val igraph = Graph.newGraph()
        val emptyLiveSet = (TempSet.empty, [])
        val emptyLiveMap: liveMap = Flow.Graph.Table.empty

        (* Tables to map temps-->nodes and nodes-> temps *)
        val tempToNode = ref Temp.Table.empty
        val nodeToTemp = ref Flow.Graph.Table.empty

        fun addTempAndNode(temp) =
          (case Temp.Table.look(!tempToNode,temp) of
              SOME(_) => ()
            | NONE =>
              let val node = Graph.newNode(igraph)
              in
                tempToNode := Temp.Table.enter(!tempToNode,temp,node);
                nodeToTemp := Flow.Graph.Table.enter(!nodeToTemp,node,temp);
                ()
              end)

        fun addTempsAndNodes(temps) =
          (case temps of
              NONE => ()
            | SOME(temps) =>
              app (fn (temp) => addTempAndNode(temp)) temps)

        fun setEmptyLiveness([],liveInMap,liveOutMap) = (liveInMap,liveOutMap)
          | setEmptyLiveness(node::nodes,liveInMap,liveOutMap) =
            (addTempsAndNodes(Flow.Graph.Table.look(def,node));
             addTempsAndNodes(Flow.Graph.Table.look(use,node));
             setEmptyLiveness(nodes,
                              Flow.Graph.Table.enter(liveInMap,node,emptyLiveSet),
                              Flow.Graph.Table.enter(liveOutMap,node,emptyLiveSet)))

        val (liveInMap:liveMap,liveOutMap:liveMap) = setEmptyLiveness(Graph.nodes(control),emptyLiveMap,emptyLiveMap)

        (* helpers *)
        fun toSet(NONE) = TempSet.empty
          | toSet(SOME(l)) = TempSet.addList(TempSet.empty,l)
        fun unpackSet(NONE)  = TempSet.empty
          | unpackSet(SOME(s,_)) = s

        (* initialize the liveness graph *)
        fun setLiveness(node,(liveInMap,liveOutMap)) =
          let val useS = toSet(Graph.Table.look(use,node))
              val outN = unpackSet(Flow.Graph.Table.look(liveOutMap,node))
              val defN = toSet(Graph.Table.look(def,node))
              val outMinusDefS = TempSet.difference(outN, defN)
              val inSet = TempSet.union(useS, outMinusDefS)
              val outSet = foldl (fn (n,s) => TempSet.union(unpackSet(Flow.Graph.Table.look(liveInMap,n)),s)) TempSet.empty (Graph.succ(node))
          in
            (Flow.Graph.Table.enter(liveInMap,node,(inSet,[])),
             Flow.Graph.Table.enter(liveOutMap,node,(outSet,[])))
          end

        (* compares whether liveIn and liveOut are the same. this info is needed
           in order to decide when to stop liveness iteration. see Algorithm 10.4 on pg 214 *)
        fun sameGraph(liveIn, liveOut) =
          let val entriesIn = Flow.Graph.Table.entries(liveIn)
              val entriesOut = Flow.Graph.Table.entries(liveOut)
              fun comp([],[]) = true
                | comp((inKey,(inValue,_))::restIn,(outKey,(outValue,_))::restOut) =
                  (case TempSet.compare(inValue,outValue) of
                      EQUAL => comp(restIn,restOut)
                    | _ => false)
                | comp _ = false
          in
            comp(entriesIn,entriesOut)
          end

        fun computeLiveness(liveIn,liveOut) =
          let val (liveIn',liveOut') = foldl (fn (node,liveness) => setLiveness(node,liveness))
                                             (liveIn,liveOut)
                                             (Graph.nodes(control))
          in
            if sameGraph(liveIn, liveIn') andalso sameGraph(liveOut, liveOut')
            then (liveIn,liveOut)
            else computeLiveness(liveIn',liveOut')
          end

        val (liveIn,liveOut) = computeLiveness(liveInMap,liveOutMap)

        (* add edges to the interference graph *)
        fun addEdges(fromNode,toTemp::temps) =
            (case Temp.Table.look(!tempToNode,toTemp) of
                NONE => raise NodeDoesNotExist
              | SOME(toNode) => (Graph.mk_edge({from=fromNode,to=toNode}));
                                 addEdges(fromNode,temps))
          | addEdges(_,[]) = ()

        fun buildInterferenceGraph(liveIn,liveOut) =
          let val defs = map (fn (node) => (node,(Graph.Table.look(def,node)))) (Graph.nodes(control))
          in
            app (fn (n,d) =>
                    (case d of
                        NONE => ()
                      | SOME(d') => addEdges(n,d')))
                defs
          end

        (* functions which map temps->nodes and nodes->temps*)
        val tnode = (fn(t) => valOf(Temp.Table.look(!tempToNode, t)))
        val gtemp = (fn(n) => valOf(Flow.Graph.Table.look(!nodeToTemp, n)))
        val tempOutList = (fn(n) => case valOf(Flow.Graph.Table.look(liveOutMap,n)) of
                                          (s,_) => TempSet.listItems(s))

    in
      buildInterferenceGraph(liveIn,liveOut);
      (IGRAPH({graph=igraph,
              tnode=tnode,
              gtemp=gtemp,
              moves=[]}),
       tempOutList)
               (* Did not calculate moves. It appears, this can be left
                            like this, but our register allocation wont be as
                            efficient as it can be*)
    end

end

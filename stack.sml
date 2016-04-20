(* Apparently stacks dont exist in any sml standard library..
   This will be useful in the register allocator *)
structure Stack =
struct
  fun empty() = ref []
  fun push(s,el) = s := el::(!s)
  fun pop(s) =
    case !s of
        [] =>  NONE
      | el::rest => (s := rest; el)
  fun printStack(s) =
    let fun printH([]) = print("End Stack\n")
          | printH((el: Graph.node)::rest) =
            (print(Graph.nodename(el) ^ "\n");
             printH(rest))
    in
      print("Start Stack:\n");
      printH(!s)
    end
end

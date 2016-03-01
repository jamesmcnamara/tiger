structure FindEscape: sig val findEscape: Absyn.exp -> unit
                      end =
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  fun traverseVar(env:escEnv, d:depth, s:Absyn.var) : unit = () (* TODO *)
  fun traverseExp(env:escEnv, d:depth, s:Absyn.var) : unit = () (* TODO *)
  (*fun traverseDecs(env, d, s: Absyn.dec list) : escEnv = ... (* TODO *)*)

  fun findEscape(prog: Absyn.exp) : unit = () (* TODO *)
end

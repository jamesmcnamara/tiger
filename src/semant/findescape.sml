structure FindEscape: sig val findEscape: Absyn.exp -> unit
                      end =
struct
  type depth = int
  type escEnv = (depth * bool ref) Symbol.table

  fun traverseVar(env:escEnv, d:depth, s:Absyn.var) : unit =
    (case s of
        Absyn.SimpleVar(sym,pos) =>
          (case Symbol.look(env,sym) of
              SOME(d',esc) => if d > d' then esc := true else ()
            | NONE => ())
      | Absyn.FieldVar(var,sym,pos) => (traverseVar(env,d,var))
      | Absyn.SubscriptVar(var,exp,pos) =>
        (traverseExp(env,d,exp);
         traverseVar(env,d,var)))

  and traverseExp(env:escEnv, d:depth, s:Absyn.exp) : unit =
    (case s of
        Absyn.VarExp(v) => traverseVar(env,d,v)
      | Absyn.NilExp => ()
      | Absyn.IntExp(_) => ()
      | Absyn.StringExp(_) => ()
      | Absyn.CallExp({args,func,pos}) =>
        foldl (fn (exp,acc) => traverseExp(env,d,exp)) () args
      | Absyn.OpExp({left,right,oper,pos}) =>
        foldl (fn (exp,acc) => traverseExp(env,d,exp)) () [left,right]
      | Absyn.RecordExp({fields,typ,pos}) =>
        foldl (fn ((symbol,exp,pos),acc) => traverseExp(env,d,exp)) () fields
      | Absyn.SeqExp(seq) =>
        foldl (fn ((exp,pos),acc) => traverseExp(env,d,exp)) () seq
      | Absyn.AssignExp({var,exp,pos}) =>
        (traverseVar(env,d,var);
         traverseExp(env,d,exp))
      | Absyn.IfExp({test,then',else',pos}) =>
        (traverseExp(env,d,test);
         traverseExp(env,d,then');
         (case else' of
              SOME(exp) => traverseExp(env,d,exp)
            | NONE => ()))
      | Absyn.WhileExp({test,body,pos}) =>
        (traverseExp(env,d,test);
         traverseExp(env,d,body))
      | Absyn.ForExp({var,escape,lo,hi,body,pos}) =>
        (escape := false; (* TODO: I think this should be false, but if so, it should be changed in the parser, not here. This applies to all of the escapes *)
         traverseExp(env,d,lo);
         traverseExp(env,d,hi);
         traverseExp(Symbol.enter(env,var,(d,escape)),d,body))
      | Absyn.BreakExp(_) => ()
      | Absyn.LetExp({decs,body,pos}) =>
        let val env' = traverseDecs(env,d,decs)
        in
          traverseExp(env',d,body)
        end
      | Absyn.ArrayExp({typ,size,init,pos}) =>
        (traverseExp(env,d,size);
         traverseExp(env,d,init)))

  and traverseDecs(env,d,[]) = env
    | traverseDecs(env,d,dec::decs) =
      let fun addToEnv([],env') = env'
            | addToEnv({name,escape,typ,pos}::params,env') =
              addToEnv(params,Symbol.enter(env',name,(d+1,escape)))
          fun traverseFuns([],env') = env'
            | traverseFuns({name,params,result,body,pos}::funs,env') =
              let val new_env = (traverseFuns(funs,addToEnv(params,env')))
              in
                (traverseExp(new_env,d+1,body);
                 new_env)
              end
      in
        case dec of
            Absyn.FunctionDec(fundecs) => (traverseFuns(fundecs,env); env)
          | Absyn.VarDec({name,escape,typ,init,pos}) =>
            traverseDecs(Symbol.enter(env,name,(d,escape)),d,decs)
          | Absyn.TypeDec(_) => traverseDecs(env,d,decs)
      end

  fun findEscape(prog: Absyn.exp) : unit = traverseExp(Symbol.empty,0,prog)
end

signature SEMANT = sig
    exception TypeError of Types.ty * Types.ty * int

    type tenv
    type venv
    type expty

    val transProg : Absyn.exp -> expty
    val transVar : tenv * venv * Absyn.var -> expty
    val transExp : tenv * venv * Absyn.exp -> expty
    val transDec : tenv * venv * Absyn.dec -> {tenv: tenv, venv: venv}
    val transTy : tenv * Absyn.ty -> Types.ty
end

structure A = Absyn
structure Semant : SEMANT = struct

exception TypeError of Types.ty * Types.ty * int
exception NotImplemented

type tenv = Types.ty Symbol.table
type venv = Env.enventry Symbol.table
type expty = {exp: Translate.exp, ty: Types.ty}

fun unify(ty1, ty2, pos) =
  if (ty1 = ty2) then
      ty1
  else
      raise TypeError(ty1, ty2, pos)

fun transVar(tenv,venv,A.SimpleVar(s,p)) = {exp=(), ty=Types.UNIT}
  | transVar(tenv,venv,A.FieldVar(v,s,p)) = {exp=(), ty=Types.UNIT}
  | transVar(tenv,venv,A.SubscriptVar(v,e,p)) = {exp=(), ty=Types.UNIT}

fun transExp(tenv, venv, exp) =
  let fun trexp(A.VarExp(v)) = transVar(tenv, venv, v)
        | trexp(A.NilExp) = {exp=(), ty=Types.NIL}
        | trexp(A.IntExp(i)) = {exp=(), ty=Types.INT}
        | trexp(A.StringExp(s, p)) = {exp=(), ty=Types.STRING}
        | trexp(A.CallExp{func, args, pos}) = {exp=(), ty=Types.UNIT}
        | trexp(A.OpExp{left,oper,right,pos}) =
          (unify(Types.INT, #ty(trexp(left)), pos);
           unify(Types.INT, #ty(trexp(right)), pos);
           {exp=(), ty=Types.INT})
        | trexp(A.RecordExp{fields,typ,pos}) = {exp=(), ty=Types.UNIT}
        | trexp(A.SeqExp l) = {exp=(), ty=Types.UNIT}
        | trexp(A.AssignExp{var=v,exp=e,pos}) = {exp=(), ty=Types.UNIT}
        | trexp(A.IfExp{test,then',else',pos}) =
          (unify(#ty(trexp(test)), Types.INT, pos);
           case else' of
               Option.SOME(e) =>
               {exp=(), ty=unify(#ty(trexp(then')), #ty(trexp(e)), pos)}
             | Option.NONE =>
               (trexp(then');
                {exp=(), ty=Types.UNIT}))
        | trexp(A.WhileExp{test,body,pos}) = {exp=(), ty=Types.UNIT}
        | trexp(A.ForExp{var=v,escape=b,lo,hi,body,pos}) = {exp=(), ty=Types.UNIT}
        | trexp(A.BreakExp p) = {exp=(), ty=Types.UNIT}
        | trexp(A.LetExp{decs,body,pos}) = {exp=(), ty=Types.UNIT}
        | trexp(A.ArrayExp{typ,size,init,pos}) = {exp=(), ty=Types.UNIT}
  in
      trexp(exp)
  end

fun transDec(tenv,venv,A.FunctionDec l) = {tenv=tenv, venv=venv}
  | transDec(tenv,venv,A.VarDec{name,escape,typ,init,pos}) = {tenv=tenv, venv=venv}
  | transDec(tenv,venv,A.TypeDec l) = {tenv=tenv, venv=venv}

fun transTy(tenv,A.NameTy(s,p)) = Types.UNIT
  | transTy(tenv,A.RecordTy l) = Types.UNIT
  | transTy(tenv,A.ArrayTy(s,p)) = Types.UNIT

fun transProg ast = transExp(Env.base_tenv,Env.base_venv,ast)

end

signature SEMANT =
sig
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
structure Semant :> SEMANT =
struct
    exception NotImplemented

    type tenv = Types.ty Symbol.table
    type venv = Env.enventry Symbol.table
    type expty = {exp: Translate.exp, ty: Types.ty}

    fun transVar(tenv,venv,A.SimpleVar(s,p)) = {exp=(), ty=Types.UNIT}
      | transVar(tenv,venv,A.FieldVar(v,s,p)) = {exp=(), ty=Types.UNIT}
      | transVar(tenv,venv,A.SubscriptVar(v,e,p)) = {exp=(), ty=Types.UNIT}

    fun transExp(tenv,venv,A.VarExp v) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.NilExp) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.IntExp i) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.StringExp(s,p)) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.CallExp{func,args,pos}) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.OpExp{left,oper=A.PlusOp,right,pos}) =
        let val {exp=_, ty=tyleft} = transExp(tenv,venv,left)
            val {exp=_, ty=tyright} = transExp(tenv,venv,right)
        in
           {exp=(), ty=Types.INT}
        end
      | transExp(tenv,venv,A.OpExp{left,oper,right,pos}) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.RecordExp{fields,typ,pos}) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.SeqExp l) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.AssignExp{var=v,exp=e,pos}) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.IfExp{test,then',else',pos}) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.WhileExp{test,body,pos}) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.ForExp{var=v,escape=b,lo,hi,body,pos}) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.BreakExp p) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.LetExp{decs,body,pos}) = {exp=(), ty=Types.UNIT}
      | transExp(tenv,venv,A.ArrayExp{typ,size,init,pos}) = {exp=(), ty=Types.UNIT}

    fun transDec(tenv,venv,A.FunctionDec l) = {tenv=tenv, venv=venv}
      | transDec(tenv,venv,A.VarDec{name,escape,typ,init,pos}) = {tenv=tenv, venv=venv}
      | transDec(tenv,venv,A.TypeDec l) = {tenv=tenv, venv=venv}

    fun transTy(tenv,A.NameTy(s,p)) = Types.UNIT
      | transTy(tenv,A.RecordTy l) = Types.UNIT
      | transTy(tenv,A.ArrayTy(s,p)) = Types.UNIT

    fun transProg ast = transExp(Env.base_tenv,Env.base_venv,ast)

end

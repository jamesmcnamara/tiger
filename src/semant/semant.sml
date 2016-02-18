signature SEMANT = sig
    exception TypeError of Types.ty * Types.ty * int
    exception TypeDoesNotExist of Symbol.symbol

    type tenv
    type venv
    type expty

    val transProg : Absyn.exp -> expty
    val transVar : tenv * venv * Absyn.var -> expty
    val transExp : tenv * venv * Absyn.exp -> expty
    val transDec : tenv * venv * Absyn.dec -> {tenv: tenv, venv: venv}
    val transTy : tenv * Absyn.ty -> Types.ty

    val checkRecordType : Types.ty list * Types.ty list * int -> bool
    val createSymbols : Absyn.field list * tenv -> (Symbol.symbol * Types.ty) list
end

structure A = Absyn
structure Semant : SEMANT = struct

exception TypeError of Types.ty * Types.ty * int
exception NotImplemented
exception TypeDoesNotExist of Symbol.symbol

type tenv = Types.ty Symbol.table
type venv = Env.enventry Symbol.table
type expty = {exp: Translate.exp, ty: Types.ty}

fun unify(ty1, ty2, pos) =
  if (ty1 = ty2) then
      ty1
  else
      raise TypeError(ty1, ty2, pos)

fun checkRecordType(fieldType::fieldTypes, recordType::recordTypes, pos) =
    (unify(fieldType,recordType,pos); checkRecordType(fieldTypes, recordTypes, pos))
    | checkRecordType(nil,nil,pos) = true
    | checkRecordType _ = false

fun createSymbols ({name=s,escape=e,typ=t,pos=p}::fields,tenv) =
      (case Symbol.look(tenv,t) of
          SOME(type') => (t,type')::createSymbols(fields,tenv)
        | NONE => raise TypeDoesNotExist(t))
    | createSymbols (nil,tenv) = nil

fun transVar(tenv,venv,A.SimpleVar(s,p)) = {exp=(), ty=Types.UNIT}
  | transVar(tenv,venv,A.FieldVar(v,s,p)) = {exp=(), ty=Types.UNIT}
  | transVar(tenv,venv,A.SubscriptVar(v,e,p)) = {exp=(), ty=Types.UNIT}

fun transTy(tenv,A.NameTy(s,p)) =
      (* BUG: Does not allow for mutually recursive type defintions. See assignment for details *)
      (case Symbol.look(tenv,s) of
            SOME(t) => t
          | NONE => raise TypeDoesNotExist(s))
    | transTy(tenv,A.RecordTy l) = Types.RECORD(List.rev(createSymbols(l,tenv)), ref ())
    | transTy(tenv,A.ArrayTy(s,p)) =
        (case Symbol.look(tenv,s) of
              SOME(t) => Types.ARRAY(t, ref ())
            | NONE => raise TypeDoesNotExist(s))

fun transTys(tenv,{name=s,ty=ty,pos=p}::decs) =
      let val t = transTy(tenv,ty)
      in
        transTys(Symbol.enter(tenv,s,t),decs)
      end
    | transTys(tenv,nil) = tenv

fun transExp(tenv, venv, exp) =
  let fun
      trexp(A.VarExp(v)) =
        transVar(tenv, venv, v)
      | trexp(A.NilExp) =
        { exp=(), ty=Types.NIL }
      | trexp(A.IntExp(i)) =
        { exp=(), ty=Types.INT }
      | trexp(A.StringExp(s, p)) =
        { exp=(), ty=Types.STRING }
      | trexp(A.CallExp { func, args, pos }) =
        (case Symbol.look(venv, func) of
             Option.SOME(Env.FunEntry { formals, result }) =>
             ((ListPair.map (fn (a, f) => unify(#ty(trexp(a)), f, 1)) (args, formals));
              { exp=(), ty=result })
           | Option.SOME(_) =>
             raise NotImplemented
           | Option.NONE =>
             raise NotImplemented)
      | trexp(A.OpExp{ left,oper,right,pos }) =
        (unify(Types.INT, #ty(trexp(left)), pos);
         unify(Types.INT, #ty(trexp(right)), pos);
         { exp=(), ty=Types.INT })
      | trexp(A.RecordExp{fields, typ, pos}) =
        (case Symbol.look(tenv, typ) of
              Option.SOME(Types.RECORD(a,b)) =>
                let val fieldTypes = map(fn (s, e, p) => (#ty(trexp(e)))) fields
                    val recordTypes = map(fn (s, t) => t) a
                 in
                    (checkRecordType(fieldTypes,recordTypes,pos); { exp=(), ty=Types.RECORD(a,b) })
                 end
            | _ => raise TypeDoesNotExist(typ)
        )
      | trexp(A.SeqExp l) =
        let val exptys = (map (fn (e, p) => trexp e) l) in
            { exp=(), ty=(#ty(List.last(exptys))) } (* BUG -- cant take last of an empty. Occurs when the body of a LetExp is () *)
        end
      | trexp(A.AssignExp { var=v, exp=e, pos }) =
        { exp=(), ty=Types.UNIT }
      | trexp(A.IfExp{test,then',else',pos}) =
        (unify(#ty(trexp(test)), Types.INT, pos);
         case else' of
             Option.SOME(e) =>
             {exp=(), ty=unify(#ty(trexp(then')), #ty(trexp(e)), pos)}
           | Option.NONE =>
             (trexp(then');
              {exp=(), ty=Types.UNIT}))
      | trexp(A.WhileExp{ test, body, pos }) =
        (* TODO *)
        { exp=(), ty=Types.UNIT }
      | trexp(A.ForExp { var=v, escape=b, lo, hi, body, pos }) =
        (* TODO *)
        { exp=(), ty=Types.UNIT }
      | trexp(A.BreakExp p) =
        (* TODO *)
        { exp=(), ty=Types.UNIT }
      | trexp(A.LetExp { decs, body, pos }) =
        let fun transDecs(ltenv,lvenv,dec::rest) =
              (case transDec(ltenv,lvenv,dec) of
                {tenv=newtenv,venv=newvenv} => transDecs(newtenv,newvenv,rest))
            | transDecs(ltenv,lvenv,nil) =  {tenv=ltenv,venv=lvenv}
            val {tenv=newtenv, venv=newvenv} = transDecs(tenv,venv,decs)
        in
            transExp(newtenv, newvenv, body)
        end
      | trexp(A.ArrayExp { typ, size, init, pos }) =
        (* TODO *)
        { exp=(), ty=Types.UNIT }
  in
      trexp(exp)
  end

and transDec(tenv,venv,A.FunctionDec l) = {tenv=tenv, venv=venv}
    | transDec(tenv,venv,A.VarDec{name,escape,typ,init,pos}) =
        (case typ of
              SOME(t) => {tenv=tenv, venv=venv}
            | NONE => {tenv=tenv,venv=Symbol.enter(venv,name,Env.VarEntry({ty=(#ty(transExp(tenv, venv, init)))}))})
    | transDec(tenv,venv,A.TypeDec l) = {tenv=transTys(tenv,l), venv=venv}

fun transProg ast = transExp(Env.base_tenv, Env.base_venv, ast)

end

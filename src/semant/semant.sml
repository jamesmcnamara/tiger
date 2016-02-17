signature SEMANT = sig
    exception TypeError of Types.ty * Types.ty * int

    type tenv
    type venv
    type expty

    val transProg : Absyn.exp -> expty
    val transExp : tenv * venv * Absyn.exp -> expty
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

fun transExp(tenv, venv, exp) =
  let fun trexp(A.VarExp(v)) =
          trvar(v)

        | trexp(A.NilExp) =
          { exp=(), ty=Types.NIL }

        | trexp(A.IntExp(i)) =
          { exp=(), ty=Types.INT }

        | trexp(A.StringExp(s, p)) =
          { exp=(), ty=Types.STRING }

        | trexp(A.CallExp { func, args, pos }) =
          (case Symbol.look(venv, func) of
                Option.SOME(Env.FunEntry { formals, result }) =>
                let fun unifier(actual, expected) =
                        unify(#ty(trexp(actual)), expected, pos)
                    val pairs = (ListPair.map unifier (args, formals))
                in
                  { exp=(), ty=result }
                end
             | Option.SOME(_) =>
               raise NotImplemented
             | Option.NONE =>
               raise NotImplemented)

        | trexp(A.OpExp{ left, oper, right, pos }) =
          let val left = trexp(left)
              val right = trexp(right)
          in
            unify(Types.INT, #ty(left), pos);
            unify(Types.INT, #ty(right), pos);
            { exp=(), ty=Types.INT }
          end

        | trexp(A.RecordExp { fields, typ, pos }) =
          (case Symbol.look(tenv, typ) of
                Option.SOME(ty as Types.RECORD(l, u)) =>
                (fields;                                (* TODO: Check the fields types *)
                 { exp=(), ty=ty })
              | Option.SOME(_) => raise NotImplemented  (* Must be record type *)
              | Option.NONE => raise NotImplemented)    (* Undefined type *)

        | trexp(A.SeqExp l) =
          let val exptys = (map (fn (e, p) => trexp e) l) in
              { exp=(), ty=(#ty(List.last(exptys))) }
          end

        | trexp(A.AssignExp { var=v, exp=e, pos }) =
          let val lhs = trvar(v)
              val rhs = trexp(e)
          in
              unify(#ty(lhs), #ty(rhs), pos);  (* Strong updates? *)
              { exp=(), ty=Types.UNIT }
          end

        | trexp(A.IfExp { test, then', else', pos }) =
          let val test = trexp(test)
              val then' = trexp(then')
          in
            unify(#ty(test), Types.INT, pos);
            case else' of
                 Option.SOME(e) =>
                 { exp=(), ty=unify(#ty(then'), #ty(trexp(e)), pos)}
               | Option.NONE =>
                  (* TODO: Unify type of then? *)
                  { exp=(), ty=Types.UNIT }
          end

        | trexp(A.WhileExp { test, body, pos }) =
          let val test = trexp(test)
              val body = trexp(body)
          in
            unify(#ty(test), Types.INT, pos);
            (* TODO: Unify body type? *)
            { exp=(), ty=Types.UNIT }
          end

        | trexp(A.ForExp { var=v, escape=b, lo, hi, body, pos }) =
          (* TODO *)
          { exp=(), ty=Types.UNIT }

        | trexp(A.BreakExp p) =
          (* TODO: Should be bottom. *)
          { exp=(), ty=Types.UNIT }

        | trexp(A.LetExp { decs, body, pos }) =
          let val { tenv=tenv', venv=venv' } = trdecs(decs)
              val body = transExp(tenv', venv', body)
          in
            body
          end

        | trexp(A.ArrayExp { typ, size, init, pos }) =
          (case Symbol.look(tenv, typ) of
                Option.SOME(ty) => { exp=(), ty=Types.ARRAY(ty, ref ()) }
              | Option.NONE => raise NotImplemented)  (* Unbound type *)

      and trvar(A.SimpleVar(s, p)) =
          {exp=(), ty=Types.UNIT}

        | trvar(A.FieldVar(v, s, p)) =
          {exp=(), ty=Types.UNIT}

        | trvar(A.SubscriptVar(v, e, p)) =
          {exp=(), ty=Types.UNIT}

      and trdec(A.FunctionDec(l)) =
          { tenv=tenv, venv=venv }  (* TODO: This is obviously wrong. *)

        | trdec(A.VarDec { name, escape, typ, init, pos }) =
          { tenv=tenv, venv=venv }  (* TODO: This is obviously wrong. *)

        | trdec(A.TypeDec(l)) =
          { tenv=tenv, venv=venv }  (* TODO: This is obviously wrong. *)

      and trdecs(decs) =
        { tenv=tenv, venv=venv }

      and trtype(A.NameTy(s, p)) =
          Types.UNIT

        | trtype(A.RecordTy(l)) =
          Types.UNIT

        | trtype(A.ArrayTy(s, p)) =
          Types.UNIT
  in
      trexp(exp)
  end

fun transProg ast = transExp(Env.base_tenv, Env.base_venv, ast)

end

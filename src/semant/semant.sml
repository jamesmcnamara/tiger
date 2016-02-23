signature SEMANT = sig
    exception TypeError of Types.ty * Types.ty * int
    exception TypeDoesNotExist of Symbol.symbol
    exception ArityError of int * int * int
    exception RecordFieldNameError of Symbol.symbol * Symbol.symbol * int

    type tenv
    type venv
    type expty

    val transProg : Absyn.exp -> expty
    val transExp : tenv * venv * Absyn.exp -> expty

    val createSymbols : Absyn.field list * tenv -> (Symbol.symbol * Types.ty) list
end

structure A = Absyn
structure Semant : SEMANT = struct

exception TypeError of Types.ty * Types.ty * int
exception NotImplemented
exception TypeDoesNotExist of Symbol.symbol
exception ArityError of int * int * int
exception RecordFieldNameError of Symbol.symbol * Symbol.symbol * int

type tenv = Types.ty Symbol.table
type venv = Env.enventry Symbol.table
type expty = {exp: Translate.exp, ty: Types.ty}

fun createSymbols (fields,tenv) =
    let
      fun aux ({name=s,escape=e,typ=t,pos=p}) =
         (case Symbol.look(tenv,t) of
          SOME(type') => (s,type')
        | NONE => (s,Types.NAME(t, ref Option.NONE)))
    in
      List.map aux fields
    end

fun actual_type(tenv, ty) =
    let
      fun maybeResolveName(s, Option.SOME(t)) =
          actual_type(tenv, t)
        | maybeResolveName(s, Option.NONE) =
          (case Symbol.look(tenv, s) of
               Option.SOME(t) => actual_type(tenv, t)
             | Option.NONE => ty)
    in
      (case ty of
            Types.NAME(s, r) => maybeResolveName(s, !r)
          | _ => ty)
    end

fun unify(tenv, ty1, ty2, pos) =
  let val ty1 = actual_type(tenv, ty1)
      val ty2 = actual_type(tenv, ty2)
  in
      if (ty1 = ty2) then
          ty1
      else
          raise TypeError(ty1, ty2, pos)
  end

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
                        unify(tenv, #ty(trexp(actual)), expected, pos)
                    val args_len = List.length(args)
                    val formals_len = List.length(formals)
                in
                  if args_len <> formals_len then
                    raise ArityError(formals_len, args_len, pos)
                  else
                    (ListPair.map unifier (args, formals));
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
            unify(tenv, Types.INT, #ty(left), pos);
            unify(tenv, Types.INT, #ty(right), pos);
            { exp=(), ty=Types.INT }
          end

        | trexp(A.RecordExp { fields, typ, pos }) =
          (case Symbol.look(tenv, typ) of
                Option.SOME(ty as Types.RECORD(l, u)) =>
                let fun unifier((s1, e1, p), (s2, t2)) =
                        if Symbol.name(s1) = Symbol.name(s2) then
                          unify(tenv, #ty(trexp(e1)), t2, pos)
                        else
                          raise RecordFieldNameError(s2, s1, p)
                    val actual_len = List.length(fields)
                    val expected_len = List.length(l)
                in
                  if actual_len <> expected_len then
                    raise ArityError(expected_len, actual_len, pos)
                  else
                    (ListPair.map unifier (fields, l));
                    { exp=(), ty=ty }
                end
              | Option.SOME(_) => raise NotImplemented  (* Must be record type => we need an appropriate error msg *)
              | Option.NONE => raise TypeDoesNotExist(typ))    (* Undefined type *)

        | trexp(A.SeqExp l) =
          let val exptys = Types.UNIT :: (map (fn (e, p) => #ty(trexp(e))) l) in
              { exp=(), ty=(List.last(exptys)) }
          end

        | trexp(A.AssignExp { var=v, exp=e, pos }) =
          let val lhs = trvar(v)
              val rhs = trexp(e)
          in
              unify(tenv, #ty(lhs), #ty(rhs), pos);  (* Strong updates? *)
              { exp=(), ty=Types.UNIT }
          end

        | trexp(A.IfExp { test, then', else', pos }) =
          let val test = trexp(test)
              val then' = trexp(then')
          in
            unify(tenv, #ty(test), Types.INT, pos);
            case else' of
                 Option.SOME(e) =>
                 { exp=(), ty=unify(tenv, #ty(then'), #ty(trexp(e)), pos)}
               | Option.NONE =>
                  { exp=(), ty=unify(tenv, #ty(then'), Types.UNIT, pos) }
          end

        | trexp(A.WhileExp { test, body, pos }) =
          let val test = trexp(test)
              val body = trexp(body)
          in
            unify(tenv, #ty(test), Types.INT, pos);
            unify(tenv, #ty(body), Types.UNIT, pos);
            { exp=(), ty=Types.UNIT }
          end

        | trexp(A.ForExp { var=v, escape=b, lo, hi, body, pos }) =
          let val venv' = Symbol.enter(venv, v, Env.VarEntry { ty=Types.INT })
              val lo' = trexp(lo)
              val hi' = trexp(hi)
              val body' = transExp(tenv, venv', body)
          in
              unify(tenv, #ty(lo'), Types.INT, pos);
              unify(tenv, #ty(hi'), Types.INT, pos);
              unify(tenv, #ty(body'), Types.UNIT, pos);
              { exp=(), ty=Types.UNIT }
          end

        | trexp(A.BreakExp p) =
          (* TODO: Should be bottom. *)
          { exp=(), ty=Types.UNIT }

        | trexp(A.LetExp { decs, body, pos }) =
          let val { tenv=tenv', venv=venv' } = trdecs(tenv, venv, decs)
              val body = transExp(tenv', venv', body)
          in
              body
          end

        | trexp(A.ArrayExp { typ, size, init, pos }) =
          (case Symbol.look(tenv, typ) of
                Option.SOME(ty) => { exp=(), ty=Types.ARRAY(ty, ref ()) }
              | Option.NONE => raise TypeDoesNotExist(typ))

      and trvar(A.SimpleVar(s, p)) =
          (case Symbol.look(venv, s) of
                Option.SOME(Env.VarEntry({ ty=ty })) =>
                { exp=(), ty=actual_type(tenv, ty) }
              | Option.SOME(Env.FunEntry(_)) =>
                raise NotImplemented
              | Option.NONE =>
                raise NotImplemented)

        | trvar(A.FieldVar(v, s, p)) =
          {exp=(), ty=Types.UNIT}

        | trvar(A.SubscriptVar(v, e, p)) =
          {exp=(), ty=Types.UNIT}

      and trdec(A.FunctionDec(l), {tenv=tenv, venv=venv}) =
          { tenv=tenv, venv=venv }  (* TODO *)

        | trdec(A.VarDec{ name, escape, typ, init, pos}, {tenv=tenv, venv=venv}) =
          (case typ of
               SOME(t) => { tenv=tenv, venv=venv }
             | NONE => { tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry({ ty=(#ty(transExp(tenv, venv, init))) })) })

        | trdec(A.TypeDec(l), {tenv=tenv, venv=venv}) =
          { tenv=trtypes(tenv, l), venv=venv }

      and trdecs(tenv, venv, decs) =
        foldl trdec {tenv=tenv, venv=venv} decs

      (* BUG: Does not allow for mutually recursive type defintions. See assignment for details *)
      and trtype(tenv, A.NameTy(rhs, p)) =
          (case Symbol.look(tenv, rhs) of
               SOME(t) => actual_type(tenv, t)
             | NONE => Types.NAME(rhs, ref (Option.NONE)))
        | trtype(tenv, A.RecordTy(l)) =
          Types.RECORD(createSymbols(l, tenv), ref ())

        | trtype(tenv, A.ArrayTy(s, p)) =
              (case Symbol.look(tenv, s) of
                   SOME(t) => Types.ARRAY(t, ref ())
                 | NONE => raise TypeDoesNotExist(s))

      and trtypes(tenv, decs) =
        let
          fun aux ({ name=s, ty=ty, pos=p }, tenv) =
            let
              val newType = trtype(tenv, ty)
            in
              Symbol.enter(tenv, s, newType)
            end
        in
          foldr aux tenv decs
        end
  in
      trexp(exp)
  end

fun transProg ast = transExp(Env.base_tenv, Env.base_venv, ast)

end

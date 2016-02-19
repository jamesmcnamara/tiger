signature SEMANT = sig
    exception TypeError of Types.ty * Types.ty * int
    exception TypeDoesNotExist of Symbol.symbol

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

type tenv = Types.ty Symbol.table
type venv = Env.enventry Symbol.table
type expty = {exp: Translate.exp, ty: Types.ty}

fun createSymbols ({name=s,escape=e,typ=t,pos=p}::fields,tenv) =
      (print("RECORD CREATE SYMBOL " ^ Symbol.name(t) ^ "\n");
      (case Symbol.look(tenv,t) of
          SOME(type') => (t,type')::createSymbols(fields,tenv)
        | NONE => (t,Types.NAME(t, ref Option.NONE))::createSymbols(fields,tenv)))
    | createSymbols (nil,tenv) = nil

fun actual_type(tenv, ty) =
  (case ty of
        Types.NAME(s, r) =>
        (print("RESOLVING " ^ Symbol.name(s) ^ "\n");
        (case !r of
              Option.SOME(t) => actual_type(tenv, t)
            | Option.NONE => (case Symbol.look(tenv, s) of
                                   Option.SOME(t) => actual_type(tenv, t)
                                 | Option.NONE => ty)))
      | _ => ty)

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
                    val pairs = (ListPair.map unifier (args, formals))  (* TODO: expected actual of different sizes. *)
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
            unify(tenv, Types.INT, #ty(left), pos);
            unify(tenv, Types.INT, #ty(right), pos);
            { exp=(), ty=Types.INT }
          end

        | trexp(A.RecordExp { fields, typ, pos }) =
          (case Symbol.look(tenv, typ) of
                Option.SOME(ty as Types.RECORD(l, u)) =>
                let fun unifier(actual, expected) =
                        unify(tenv, #ty(trexp(actual)), expected, pos)
                    val actual = map(fn (s, e, p) => (e)) fields
                    val expected = map(fn (s, t) => t) l
                    val pairs = (ListPair.map unifier (actual, expected))  (* TODO: expected actual of different sizes, orders. *)
                in
                  { exp=(), ty=ty }
                end
              | Option.SOME(_) => raise NotImplemented  (* Must be record type *)
              | Option.NONE => raise NotImplemented)    (* Undefined type *)

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
                  (* TODO: Unify type of then? *)
                  { exp=(), ty=Types.UNIT }
          end

        | trexp(A.WhileExp { test, body, pos }) =
          let val test = trexp(test)
              val body = trexp(body)
          in
            unify(tenv, #ty(test), Types.INT, pos);
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
          let val { tenv=tenv', venv=venv' } = trdecs(tenv, venv, decs)
              val body = transExp(tenv', venv', body)
          in
              body
          end

        | trexp(A.ArrayExp { typ, size, init, pos }) =
          (case Symbol.look(tenv, typ) of
                Option.SOME(ty) => { exp=(), ty=Types.ARRAY(ty, ref ()) }
              | Option.NONE => raise NotImplemented)  (* Unbound type *)

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

      and trdec(tenv, venv, A.FunctionDec(l)) =
          { tenv=tenv, venv=venv }  (* TODO *)

        | trdec(tenv, venv, A.VarDec{ name, escape, typ, init, pos}) =
          (case typ of
               SOME(t) => { tenv=tenv, venv=venv }
             | NONE => { tenv=tenv, venv=Symbol.enter(venv, name, Env.VarEntry({ ty=(#ty(transExp(tenv, venv, init))) })) })

        | trdec(tenv, venv, A.TypeDec(l)) =
          { tenv=trtypes(tenv, l), venv=venv }

      and trdecs(tenv, venv, dec::rest) =
          let val { tenv=tenv', venv=venv' } = trdec(tenv, venv, dec) in
            trdecs(tenv', venv', rest)
          end
        | trdecs(tenv, venv, nil) = { tenv=tenv, venv=venv }

      (* BUG: Does not allow for mutually recursive type defintions. See assignment for details *)
      and trtype(tenv, lhs, A.NameTy(rhs, p)) =
          (case Symbol.look(tenv, rhs) of
               SOME(t) => Symbol.enter(tenv, lhs, actual_type(tenv, t))
             | NONE => let val t1 = Types.NAME(rhs, ref (Option.NONE))
                           val t2 = Types.NAME(lhs, ref (Option.SOME(t1)))
                           val tenv' = Symbol.enter(tenv, rhs, t1)
                           val tenv' = Symbol.enter(tenv', lhs, t2)
                           (* TODO: What if the type was already declared? *)
                       in
                          tenv'
                       end)
        | trtype(tenv, lhs, A.RecordTy(l)) =
          Symbol.enter(tenv, lhs, Types.RECORD(List.rev(createSymbols(l, tenv)), ref ()))

        | trtype(tenv, lhs, A.ArrayTy(s, p)) =
              Symbol.enter(tenv, lhs, (case Symbol.look(tenv, s) of
                   SOME(t) => Types.ARRAY(t, ref ())
                 | NONE => raise TypeDoesNotExist(s)))

      and trtypes(tenv, { name=s, ty=ty, pos=p } :: decs) =
          let val tenv' = trtype(tenv, s, ty) in
              trtypes(tenv', decs)
            end
        | trtypes(tenv, nil) = tenv
  in
      trexp(exp)
  end

fun transProg ast = transExp(Env.base_tenv, Env.base_venv, ast)

end

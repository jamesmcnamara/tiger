signature SEMANT = sig
    exception TypeError of Types.ty * Types.ty * int
    exception TypeDoesNotExist of Symbol.symbol
    exception ArityError of int * int * int
    exception RecordFieldNameError of Symbol.symbol * Symbol.symbol * int
    exception FunctionIsNotValueError of Symbol.symbol * int
    exception UndefinedId of Symbol.symbol * int
    exception NotRecordError of Types.ty * int
    exception FieldNotFound of Types.ty * Symbol.symbol * int
    exception NotArrayError of Types.ty * int
    exception OperatorError of Types.ty * int

    type tenv
    type venv
    type expty

    val transProg : Absyn.exp -> expty
    val transExp : tenv * venv * Absyn.exp * Translate.level * Temp.label option -> expty

    val createSymbols : Absyn.field list * tenv -> (Symbol.symbol * Types.ty) list
end

structure A = Absyn
structure T = Tree
structure Semant : SEMANT = struct

exception TypeError of Types.ty * Types.ty * int
exception NotImplemented
exception TypeDoesNotExist of Symbol.symbol
exception ArityError of int * int * int
exception RecordFieldNameError of Symbol.symbol * Symbol.symbol * int
exception FunctionIsNotValueError of Symbol.symbol * int
exception UndefinedId of Symbol.symbol * int
exception NotRecordError of Types.ty * int
exception FieldNotFound of Types.ty * Symbol.symbol * int
exception NotArrayError of Types.ty * int
exception OperatorError of Types.ty * int

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
          | Types.ARRAY(t, u) => actual_type(tenv, t)
          | _ => ty)
    end

fun unify_actual(tenv, Types.BOTTOM, t, pos) = t
  | unify_actual(tenv, t, Types.BOTTOM, pos) = t
  | unify_actual(tenv, Types.NIL, Types.RECORD(l, u), pos) = Types.RECORD(l,u)
  | unify_actual(tenv, Types.RECORD(l, u), Types.NIL, pos) = Types.RECORD(l,u)
  | unify_actual(tenv, ty1, ty2, pos) =
      if (ty1 = ty2) then
          ty1
      else
          raise TypeError(ty1, ty2, pos)

fun unify(tenv, ty1, ty2, pos) =
    let val ty1 = actual_type(tenv, ty1)
        val ty2 = actual_type(tenv, ty2)
    in
        unify_actual(tenv, ty1, ty2, pos)
    end

fun transExp(tenv, venv, exp, level, join) =
  let fun
          (* Varibale expressions.
           ***********************)
          trexp(A.VarExp(v)) =
            trvar(v)

          (* `nil` expressions.
           ********************)
        | trexp(A.NilExp) =
          { exp=Translate.Ex(T.CONST(0)), ty=Types.NIL }

          (* Integer expressions.
           **********************)
        | trexp(A.IntExp(i)) =
          { exp=Translate.Ex(T.CONST(i)), ty=Types.INT }

          (* String expressions.
           *********************)
        | trexp(A.StringExp(s, p)) =
          { exp=Translate.string(s), ty=Types.STRING }

          (* Call expressions.
           *******************)
        | trexp(A.CallExp { func, args, pos }) =
          (case Symbol.look(venv, func) of
            (* The call was on a symbol defined as a function. *)
            Option.SOME(Env.FunEntry { formals, result, level, label }) =>
              let
                fun unifier(actual, expected) =
                    unify(tenv, #ty(trexp(actual)), expected, pos)
                val args_len = List.length(args)
                val formals_len = List.length(formals)
              in
                if args_len <> formals_len then
                  raise ArityError(formals_len, args_len, pos)
                else
                  (ListPair.map unifier (args, formals));
                  { exp=Translate.Dx, ty=result }
              end
            (* The call was on a symbol defined as a variable. *)
          | Option.SOME(_) =>
              raise FunctionIsNotValueError(func, pos)
            (* The call was on an undefined symbol. *)
          | Option.NONE =>
              raise UndefinedId(func, pos))

          (* Operation expressions.
           ************************)
        | trexp(A.OpExp{ left, oper, right, pos }) =
          let
            val left = trexp(left)
            val right = trexp(right)
            fun trarithop() =
              (unify(tenv, Types.INT, #ty(left), pos);
               unify(tenv, Types.INT, #ty(right), pos);
               { exp=Translate.arithop(oper, #exp left, #exp right), ty=Types.INT })
            fun trcmpop() =
              (case #ty(left) of
                Types.INT =>
                (unify(tenv, Types.INT, #ty(right), pos);
                 { exp=Translate.arithop(oper,#exp left,#exp right), ty=Types.INT })
              | Types.STRING =>
                (unify(tenv, Types.STRING, #ty(right), pos);
                 { exp=Translate.stringop(oper,#exp left,#exp right), ty=Types.INT })
              | _ =>
                raise OperatorError(#ty(left), pos))
            fun treqop() =
              (case #ty(left) of
                Types.INT =>
                (unify(tenv, Types.INT, #ty(right), pos);
                 { exp=Translate.arithop(oper,#exp left,#exp right), ty=Types.INT })
              | Types.STRING =>
                (unify(tenv, Types.STRING, #ty(right), pos);
                 { exp=Translate.stringop(oper,#exp left,#exp right), ty=Types.INT })
              | Types.RECORD(l, u) =>
                (unify(tenv, Types.RECORD(l, u), #ty(right), pos);
                 { exp=Translate.arithop(oper,#exp left,#exp right), ty=Types.INT})
              | Types.ARRAY(t, u) =>
                (unify(tenv, Types.ARRAY(t, u), #ty(right), pos);
                 { exp=Translate.arithop(oper,#exp left,#exp right), ty=Types.INT})
              | _ =>
                raise OperatorError(#ty(left), pos))
          in
            (case oper of
              A.PlusOp   => trarithop()
            | A.MinusOp  => trarithop()
            | A.DivideOp => trarithop()
            | A.TimesOp  => trarithop()
            | A.EqOp     => treqop()
            | A.NeqOp    => treqop()
            | A.LtOp     => trcmpop()
            | A.LeOp     => trcmpop()
            | A.GtOp     => trcmpop()
            | A.GeOp     => trcmpop())
          end

          (* Record expressions.
           *********************)
        | trexp(A.RecordExp { fields, typ, pos }) =
          (case Symbol.look(tenv, typ) of
            Option.SOME(ty as Types.RECORD(expected_fields, unique)) =>
              let
                val len = List.length(fields)
                val expected_len = List.length(expected_fields)
                fun unifier((s1, e1, p), (s2, t2)) =
                    if Symbol.name(s1) = Symbol.name(s2) then
                      unify(tenv, #ty(trexp(e1)), t2, pos)
                    else
                      raise RecordFieldNameError(s2, s1, p)
              in
                if len <> expected_len then
                  raise ArityError(expected_len, len, pos)
                else
                  (* NOTE: Mapping the fields in order is valid because Tiger
                   * records are ordered. *)
                  (ListPair.map unifier (fields, expected_fields));
                  { exp=Translate.Dx, ty=ty }
              end
            | Option.SOME(_) =>
              (* TODO: Must be record type. We need an appropriate error msg. *)
              raise NotImplemented
            | Option.NONE =>
              raise TypeDoesNotExist(typ))

          (* Sequence expressions.
           ***********************)
        | trexp(A.SeqExp l) =
          let
            val trans = (map (fn (e, p) => trexp(e)) l)
            val exptys = Types.UNIT::(map (fn (e) => #ty(e)) trans)
            val exps = (map (fn (e) => #exp(e)) trans)
          in
              { exp=Translate.sequence(exps), ty=(List.last(exptys)) }
          end

          (* Assign expressions.
           ***********************)
        | trexp(A.AssignExp { var=v, exp=e, pos }) =
          let
            val lhs = trvar(v)
            val rhs = trexp(e)
          in
            unify(tenv, #ty(lhs), #ty(rhs), pos);
            { exp=Translate.assign((#exp lhs), (#exp rhs)), ty=Types.UNIT }
          end

          (* If expressions.
           *****************)
        | trexp(A.IfExp { test, then', else', pos }) =
          let
            val test = trexp(test)
            val then' = trexp(then')
          in
            unify(tenv, #ty(test), Types.INT, pos);
            (case else' of
              Option.SOME(e) =>
                let val e' = trexp(e)
                in
                  { exp=Translate.ifthenelse(#exp (test),#exp (then'),#exp (e')),
                    ty=unify(tenv, #ty(then'), #ty(e'), pos)}
                end
            | Option.NONE =>
              { exp=Translate.ifthen(#exp (test),#exp (then')), ty=unify(tenv, #ty(then'), Types.UNIT, pos) })
          end

          (* While expressions.
           ********************)
        | trexp(A.WhileExp { test, body, pos }) =
          let
            val test = trexp(test)
            val join = Temp.newlabel()
            val body' = transExp(tenv, venv, body, level, SOME join)
          in
            unify(tenv, #ty(test), Types.INT, pos);
            unify(tenv, #ty(body'), Types.UNIT, pos);
            { exp=Translate.while'(#exp (test),#exp (body'),join), ty=Types.UNIT }
          end

          (* For expressions.
           ******************)
        | trexp(A.ForExp { var=v, escape=b, lo, hi, body, pos }) =
          let
            val venv' = Symbol.enter(venv, v, Env.VarEntry { ty=Types.INT, access=(Translate.allocLocal level (!b)) })
            val lo' = trexp(lo)
            val hi' = trexp(hi)
            val join = Temp.newlabel()
            val body' = transExp(tenv, venv', body, level, SOME join)
          in
            unify(tenv, #ty(lo'), Types.INT, pos);
            unify(tenv, #ty(hi'), Types.INT, pos);
            unify(tenv, #ty(body'), Types.UNIT, pos);
            { exp=Translate.for'(#exp (lo'),#exp (hi'),#exp (body'),join), ty=Types.UNIT }
          end

          (* Break expression.
           *******************)
        | trexp(A.BreakExp p) =
          { exp=Translate.break'(join), ty=Types.BOTTOM }

          (* Let expressions.
           ******************)
        | trexp(A.LetExp { decs, body, pos }) =
          let
            val { tenv=tenv', venv=venv', inits=inits } = trdecs(tenv, venv, decs, level)
            val trans_body = transExp(tenv', venv', body, level, join)
          in
            { exp=Translate.let'(inits, (#exp trans_body)), ty=(#ty trans_body)}
          end

          (* Array expressions.
           ********************)
        | trexp(A.ArrayExp { typ, size, init, pos }) =
          (case Symbol.look(tenv, typ) of
            Option.SOME(ty) =>
            let
                val trans_size = trexp(size)
                val trans_init = trexp(init)
            in
                unify(tenv, Types.INT, (#ty trans_size), pos);
                unify(tenv, ty, (#ty trans_init), pos);
                { exp=Translate.array((#exp trans_size), (#exp trans_init)), ty=Types.ARRAY(ty, ref ()) }
            end
          | Option.NONE =>
            raise TypeDoesNotExist(typ))

      and
          (* Simple variables.
           *******************)
          trvar(A.SimpleVar(s, p)) =
          (case Symbol.look(venv, s) of
            Option.SOME(Env.VarEntry({ ty=ty, access=access })) =>
              { exp=Translate.simpleVar(level,access), ty=actual_type(tenv, ty) }
          | Option.SOME(Env.FunEntry(_)) =>
            raise FunctionIsNotValueError(s, p)
          | Option.NONE =>
            raise UndefinedId(s, p))

          (* Field variables `posn.x`.
           ***************************)
        | trvar(A.FieldVar(v, s1, p)) =
          (case #ty(trvar(v)) of
            t as Types.RECORD(l, u) =>
            (case (List.find (fn (s2, t) => s1 = s2) l) of
              SOME(s, t) =>
              { exp=Translate.Dx, ty=t }
            | NONE =>
              raise FieldNotFound(t, s1, p))
          | t =>
            raise NotRecordError(t, p))

          (* Subuscript variable `buff[i+1]`.
           **********************************)
        | trvar(A.SubscriptVar(v, e, p)) =
          (case #ty(trexp(e)) of
            Types.INT =>
            (case trvar(v) of
              { exp=exp, ty=Types.ARRAY(t,u) } =>
              { exp=exp, ty=actual_type(tenv, t) }
            | { exp=_, ty=ty } =>
              raise NotArrayError(ty, p))
          | t =>
            raise TypeError(Types.INT, t, p))

      and
          (* Function declarations.
           ************************)
          trdec(A.FunctionDec(l), {tenv=tenv, venv=venv, inits=inits}) =
          let
            (* TODO: Clean up this function. *)
            fun insert({ name, params, result, body, pos }, venv') =
              let
                val resultType =
                  (case result of
                    SOME(s, p) =>
                    (case Symbol.look(tenv, s) of
                      SOME(t) => t
                    | NONE => raise TypeDoesNotExist(s))
                  | NONE => Types.UNIT)
                val formals =
                  (map (fn { name, escape, typ, pos } =>
                    (case Symbol.look(tenv, typ) of
                      SOME(t) =>
                      t
                    | NONE =>
                      raise TypeDoesNotExist(typ)))
                    params)
                val entry =
                  Env.FunEntry { formals=formals, result=resultType, level=level, label=Temp.newlabel() }
              in
                Symbol.enter(venv', name, entry)
              end
            val venv' = (foldr insert venv l)
            fun unifier { name, params, result, body, pos } =
              let
                fun insert({ name, escape, typ, pos }, venv'') =
                  (case Symbol.look(tenv, typ) of
                    SOME(t) =>
                    Symbol.enter(venv'', name, Env.VarEntry { ty=t, access=(Translate.allocLocal level (!escape)) })
                  | NONE =>
                    raise TypeDoesNotExist(typ))
                val venv'' = (foldr insert venv' params)
              in
                (case Symbol.look(venv', name) of
                  SOME(Env.FunEntry({ formals, result, level, label })) =>
                  unify(tenv, result, (#ty(transExp(tenv, venv'', body, level, join))), pos)
                | SOME _ =>
                  raise FunctionIsNotValueError(name, pos)
                | NONE =>
                  raise UndefinedId(name, pos))
              end
          in
            (map unifier l);
            { tenv=tenv, venv=venv', inits=inits }
          end

          (* Varibale declarations.
           ************************)
        | trdec(A.VarDec { name, escape, typ, init, pos },
                { tenv=tenv, venv=venv, inits=inits }) =
          let
            val trans = transExp(tenv, venv, init, level, join)
            val access = Translate.allocLocal level (!escape)
            val varinit = Translate.varInit(level, access, (#exp trans))
            val entry = Env.VarEntry { ty=(#ty trans), access=access }
            val venv' = Symbol.enter(venv, name, entry)
          in
            (case typ of
              SOME((s, p)) =>
              (case Symbol.look(tenv, s) of
                SOME(t) =>
                (unify(tenv, t, (#ty trans), pos);
                 { tenv=tenv, venv=venv', inits=varinit::inits })
              | NONE => raise TypeDoesNotExist(s))
            | NONE =>
              { tenv=tenv, venv=venv', inits=varinit::inits })
          end

          (* Type declarations.
           ********************)
        | trdec(A.TypeDec(l), {tenv=tenv, venv=venv, inits=inits}) =
          { tenv=trtypes(tenv, l), venv=venv, inits=inits }

      (* Helper functions. *)

      and trdecs(tenv, venv, decs, level) =
        (foldl trdec {tenv=tenv, venv=venv, inits=[]} decs)

      and trtype(tenv, A.NameTy(rhs, p)) =
          (case Symbol.look(tenv, rhs) of
            SOME(t) =>
            actual_type(tenv, t)
          | NONE =>
            Types.NAME(rhs, ref (Option.NONE)))
        | trtype(tenv, A.RecordTy(l)) =
          Types.RECORD(createSymbols(l, tenv), ref ())

        | trtype(tenv, A.ArrayTy(s, p)) =
          (case Symbol.look(tenv, s) of
            SOME(t) =>
            Types.ARRAY(t, ref ())
          | NONE =>
            raise TypeDoesNotExist(s))

      and trtypes(tenv, decs) =
          let
            fun aux ({ name=s, ty=ty, pos=p }, tenv) =
              Symbol.enter(tenv, s, trtype(tenv, ty))
          in
            foldr aux tenv decs
          end
  in
      trexp(exp)
  end

fun transProg ast =
  let val newLevel = Translate.newLevel({parent=Translate.outermost, name=Temp.newlabel(), formals=[]})
  in
    transExp(Env.base_tenv, Env.base_venv, ast, newLevel, NONE)
  end

end

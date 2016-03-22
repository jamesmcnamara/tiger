signature TRANSLATE =
sig
  type level
  type access

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm
               | Dx

  exception TypeCheckFailed

  val outermost : level
  val newLevel : {parent: level, name: Temp.label,
                  formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access

  val unEx: exp -> Tree.exp
  val unNx: exp -> Tree.stm
  val unCx: exp -> (Temp.label * Temp.label -> Tree.stm)
  (*
  val procEntryExit : {level: level, body: exp} -> unit
  val getResult : unit -> Frame.frag list



  val simpleVar: access * level -> exp
  *) (* TODO: Implement these... *)
  val arithop: Absyn.oper * exp * exp -> exp
  val stringop: Absyn.oper * exp * exp -> exp
end

structure Translate : TRANSLATE = struct
  datatype level = outermost | inner of {parent: level, frame: Frame.frame}
  type access = level * Frame.access
  exception OutermostError
  exception TypeCheckFailed
  exception BadType

  structure Frame = MipsFrame
  structure T = Tree

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm
               | Dx

  fun newLevel {parent, name, formals} =
  let
    val parent = outermost
    val frame = Frame.newFrame({formals=true::formals,name=name})
  in
    inner({parent=parent, frame=frame})
  end

  fun formals level =
    case level of
        outermost => []
      | inner {parent, frame} =>
        foldl (fn (access,acc) =>
                  (inner({parent=parent, frame=frame}), access)::acc)
              []
              (Frame.formals(frame))

  fun allocLocal level escape =
    case level of
        outermost => raise OutermostError
      | inner {parent, frame} => (level, Frame.allocLocal frame escape)

  fun seq [] = T.EXP(T.CONST(0))
    | seq [stm] = stm
    | seq (stm::stms) = T.SEQ(stm, seq(stms))

  fun unEx(Ex e) = e
    | unEx(Cx genstm) =
        let val r = Temp.newtemp()
            val t = Temp.newlabel() and f = Temp.newlabel()
        in T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
                      genstm(t,f),
                      T.LABEL f,
                      T.MOVE(T.TEMP r, T.CONST 0),
                      T.LABEL t],
                  T.TEMP r)
        end
    | unEx(Nx s) = T.ESEQ(s,T.CONST 0)

  fun unNx(Nx s) = s
    | unNx(Cx genstm) = T.EXP(unEx(Cx genstm))
    | unNx(Ex e) = T.EXP(e)

  fun unCx(Cx genstm) = genstm
    | unCx(Nx s) = raise TypeCheckFailed
    | unCx(Ex(T.CONST 0)) = (fn (t,f) => T.JUMP(T.NAME(f), [f]))
    | unCx(Ex(T.CONST 1)) = (fn (t,f) => T.JUMP(T.NAME(f), [t]))
    | unCx(Ex e) = (fn (t,f) => T.CJUMP(T.EQ, e, T.CONST(1), t, f))

  fun arithop(Absyn.PlusOp,left,right) = Ex(T.BINOP(T.PLUS, unEx(left), unEx(right)))
    | arithop(Absyn.MinusOp,left,right) = Ex(T.BINOP(T.MINUS, unEx(left), unEx(right)))
    | arithop(Absyn.TimesOp,left,right) = Ex(T.BINOP(T.MUL, unEx(left), unEx(right)))
    | arithop(Absyn.DivideOp,left,right) = Ex(T.BINOP(T.DIV, unEx(left), unEx(right)))
    | arithop(Absyn.EqOp,left,right) = Cx(fn(t,f) => T.CJUMP(T.EQ,unEx(left),unEx(right),t,f))
    | arithop(Absyn.NeqOp,left,right) = Cx(fn(t,f) => T.CJUMP(T.NE,unEx(left),unEx(right),t,f))
    | arithop(Absyn.LtOp,left,right) = Cx(fn(t,f) => T.CJUMP(T.LT,unEx(left),unEx(right),t,f))
    | arithop(Absyn.LeOp,left,right) = Cx(fn(t,f) => T.CJUMP(T.LE,unEx(left),unEx(right),t,f))
    | arithop(Absyn.GtOp,left,right) = Cx(fn(t,f) => T.CJUMP(T.GT,unEx(left),unEx(right),t,f))
    | arithop(Absyn.GeOp,left,right) = Cx(fn(t,f) => T.CJUMP(T.GE,unEx(left),unEx(right),t,f))

  fun stringop(Absyn.EqOp,left,right) = Ex(Frame.externalCall("stringEqual", [unEx(left),unEx(right)]))
    | stringop(Absyn.NeqOp,left,right) = Ex(Frame.externalCall("stringNotEqual", [unEx(left),unEx(right)]))


end

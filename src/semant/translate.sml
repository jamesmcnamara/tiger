signature TRANSLATE =
sig
  type level
  type access

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm
               | Dx

  exception TypeCheckFailed
  exception InvalidBreak

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
  val string: string -> exp
  val ifthenelse: exp * exp * exp -> exp
  val ifthen: exp * exp -> exp
  val while': exp * exp * Temp.label -> exp
  val for': exp * exp * exp * Temp.label -> exp
  val break': Temp.label option -> exp
  val sequence: exp list -> exp
end

structure Translate : TRANSLATE = struct
  datatype level = outermost | inner of {parent: level, frame: Frame.frame}
  type access = level * Frame.access
  exception OutermostError
  exception TypeCheckFailed
  exception BadType
  exception InvalidBreak

  structure Frame = MipsFrame
  structure T = Tree

  datatype exp = Ex of Tree.exp
               | Nx of Tree.stm
               | Cx of Temp.label * Temp.label -> Tree.stm
               | Dx

  val frags = ref [] : Frame.frag list ref

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

  fun string(s) =
    let val label = Temp.newlabel()
    in
      (frags := Frame.STRING(label,s)::(!frags);
       Ex(Tree.NAME(label)))
    end

  fun ifthenelse(e1,e2,e3) =
    let val t = Temp.newlabel()
        val f = Temp.newlabel()
        val join = Temp.newlabel()
        val r = Temp.newtemp()
    in
      Ex(Tree.ESEQ(seq([T.CJUMP(T.EQ, unEx(e1), T.CONST(1), t, f),
                        T.LABEL(t),
                        T.MOVE(T.TEMP(r), unEx(e2)),
                        T.JUMP(T.NAME(join), [join]),
                        T.LABEL(f),
                        T.MOVE(T.TEMP(r), unEx(e3)),
                        T.LABEL(join)
        ]), T.TEMP(r)))
    end

  fun ifthen(e1,e2) =
    let val t = Temp.newlabel()
        val f = Temp.newlabel()
        val join = Temp.newlabel()
    in
      Nx(Tree.EXP(Tree.ESEQ(seq([T.CJUMP(T.EQ, unEx(e1), T.CONST(1), t, f),
                                 T.LABEL(t),
                                 unNx(e2),
                                 T.JUMP(T.NAME(join), [join]),
                                 T.LABEL(f),
                                 T.LABEL(join)]),
                            T.CONST(0))))
    end

  fun while'(c,b,join) =
    let val test = Temp.newlabel()
        val body = Temp.newlabel()
    in
      Nx(Tree.EXP(Tree.ESEQ(seq([T.LABEL(test),
                                 T.CJUMP(T.NE, unEx(c), T.CONST(1), join, body),
                                 T.LABEL(body),
                                 unNx(b),
                                 T.JUMP(T.NAME(test), [test]),
                                 T.LABEL(join)]),
                            T.CONST(0))))
    end

  fun for'(lo,hi,body,join) =
    let val loR = T.TEMP(Temp.newtemp())
        val hiR = T.TEMP(Temp.newtemp())
        val start = Temp.newlabel()
    in
      Nx(Tree.EXP(Tree.ESEQ(seq([T.CJUMP(T.LT, loR, hiR, start, join),
                                 T.LABEL(start),
                                 unNx(body),
                                 T.MOVE(T.MEM(loR), T.BINOP(T.PLUS, T.MEM(loR), T.CONST(1))),
                                 T.CJUMP(T.LT, loR, hiR, start, join),
                                 T.LABEL(join)]),
                            T.CONST(0))))
    end

  fun break' join =
    case join of
        NONE => raise InvalidBreak
      | SOME(j) => Nx(Tree.EXP(Tree.ESEQ(seq([T.JUMP(T.NAME(j), [j])]),
                                         T.CONST(0))))

  fun sequence(l: exp list) =
    Ex(Tree.ESEQ(l))
end

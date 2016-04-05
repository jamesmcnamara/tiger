signature CODEGEN =
sig
  structure Frame : FRAME
  val codegen : Frame.frame -> Tree.stm -> Assem.instr list
end

structure CodeGen : CODEGEN =
struct
  structure Frame = Frame (* or MipsFrame *)
  structure T = Tree
  structure A = Assem

  (* TODO: this code assumes the MIPS instruction set from the MARS emulator.
  * The ISA for SPIM may or may not be a bit different. SPIM p. 14*)
  fun binopcode (oper: T.binop, immediate) =
  let
    val code =
      case oper of
           T.PLUS => "add"
         | T.MINUS => "sub"
         | T.MUL => "mul"
         | T.DIV => "div"
         | T.AND => "and"
         | T.OR  => "or"
         | T.LSHIFT => "shl"
         | T.RSHIFT => "shr"
         | T.ARSHIFT => "shra"
         | T.XOR => "xor"
    val suffix = if immediate then "i" else ""
  in
    code ^ suffix
  end

  fun relopcode T.EQ = "beq"
    | relopcode T.NE = "bne"
    | relopcode T.LT = "blt"
    | relopcode T.GT = "bgt"
    | relopcode T.LE = "ble"
    | relopcode T.GE = "bge"
    | relopcode T.ULT = "bltu"
    | relopcode T.ULE = "bleu"
    | relopcode T.UGT = "bgtu"
    | relopcode T.UGE = "bgeu"


  fun codegen frame tree =
  let
    val instList = ref (nil: A.instr list)

    fun emit instruction = instList := instruction::(!instList)

    fun result gen = let val t = Temp.newtemp() in emit(gen t); t end

    fun munchStm(T.SEQ(first, rest)) =
          (munchStm first; munchStm rest)

      | munchStm(T.LABEL(lab)) =
          emit(A.LABEL{assem=Symbol.name lab ^ ":\n", lab=lab})

      (* Load instruction *)
      | munchStm(T.MOVE(T.TEMP(t1), T.MEM(T.BINOP(T.PLUS, T.CONST(offset), e)))) =
          emit(A.OPER {assem="lw `d0, " ^ Int.toString offset ^ "(`s0`)\n",
                       dst=[t1], src=[munchExp e], jump=NONE})

      | munchStm(T.MOVE(t as T.TEMP(_), T.MEM(T.BINOP(T.PLUS, e, offset as T.CONST(_))))) =
          munchStm(T.MOVE(t, T.MEM(T.BINOP(T.PLUS, offset, e))))

      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST(offset), e)), T.TEMP(t))) =
          emit(A.OPER {assem="sw `s0, " ^ Int.toString offset ^ "(`d0`)\n",
                       dst=[munchExp e], src=[t],
                       jump=NONE})

      | munchStm(T.MOVE(d as T.TEMP(_), T.MEM(e))) =
          munchStm(T.MOVE(d, T.MEM(T.BINOP(T.PLUS, T.CONST(0), e))))

      (* Store instruction *)
      | munchStm(T.MOVE(T.MEM(e), d as T.TEMP(_))) =
          munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST(0), e)), d))

      (* Register-register move *)
      | munchStm(T.MOVE(e1, e2)) =
          emit(A.MOVE{assem="move `d0, `s0\n", dst=munchExp e1, src=munchExp e2})

      (* Jumps *)
      | munchStm(T.JUMP(T.NAME(l), labs)) =
          emit(A.OPER {assem="j `j0 \n",
                       dst=[], src=[],
                       jump=SOME(l::labs)})

      | munchStm(T.JUMP(e, labs)) =
          emit(A.OPER {assem="jr `s0 \n",
                       dst=[], src=[munchExp e],
                       jump=SOME(labs)})

      | munchStm(T.CJUMP(relop, a, b, t, f)) =
          (emit(A.OPER {assem=relopcode relop ^ "`s0, `s1, `j0\n",
                        dst=[], src=[munchExp a, munchExp b],
                        jump=SOME([t])});
           emit(A.OPER {assem="j `j0\n",
                        dst=[], src=[],
                        jump=SOME([f])}))

      (* Not sure about matching the name in the pattern. In the book, they
      * store the result in a temp and load it out of that, but I'm not sure
      * if that's the best strategy for mips. p. 204 *)
      | munchStm(T.EXP(T.CALL(T.NAME(lab), args))) =
          emit(A.OPER {assem="jal `j0\n",
                       src=munchArgs(0, args),
                       (* TODO: caller-save registers? Explanation on page
                        * 204-205 *)
                       dst=[MipsFrame.RA, MipsFrame.RV],
                       jump=SOME([lab])})

      | munchStm(T.EXP(e)) =
          emit(A.OPER {assem="li `d0, `s0\n",
                       src=[munchExp e],
                       dst=[Temp.newtemp()],
                       jump=NONE})

    and munchExp(T.BINOP(oper, e1, e2)) =
          munchBinop(oper, e1, e2)

      | munchExp(T.MEM(e)) =
          result(fn temp =>
            A.OPER {assem="lw `d0, 0(`s0)\n",
                    dst=[temp], src=[munchExp e],
                    jump=NONE})

      | munchExp(T.ESEQ(stm, exp)) =
          (munchStm stm; munchExp exp)

      | munchExp(T.TEMP(t)) = t

      | munchExp(T.NAME(lab)) =
          result(fn temp =>
            A.LABEL {assem=Symbol.name lab, lab=lab})

      (* ESEQS will be removed by canonicalization *)
      | munchExp(T.CONST(e)) =
          result(fn temp =>
            A.OPER {assem="li `d0, "^ Int.toString e ^"\n",
                    dst=[temp], src=[],
                    jump=NONE})

    and munchBinop(oper, e, T.CONST(c)) =
          result(fn temp =>
            A.OPER {assem=binopcode(oper, true) ^ " `d0, `s0, " ^ Int.toString c ^ "\n",
                    dst=[temp], src=[munchExp e],
                    jump=NONE})

      | munchBinop(oper, e1, e2) =
          result(fn temp =>
            A.OPER {assem=binopcode(oper, false) ^ " `d0, `s0, `s1\n",
                    dst=[temp], src=[munchExp e1, munchExp e2],
                    jump=NONE})

    (* TODO: this code creates registers for all the arguments to a function
    * call (for liveness analysis) and as a side effect, loads the code
    * according to the calling conventions. At this point, it assumes infinite
    * argument registers and pushes nothing onto the stack *)
    and munchArgs(i, fst::rest) =
      result(fn temp =>
        A. OPER {assem="move $a" ^ Int.toString i ^ ", `s0\n",
                 dst=[temp], src=[munchExp fst],
                 jump=NONE})::munchArgs(i + 1, rest)
      | munchArgs(i, []) = []

    in
        munchStm tree;
        rev (!instList)
  end
    (*
    * fun normalizeStm(T.MOVE(T.TEMP(t1), T.MEM(T.BINOP(T.PLUS, e, T.CONST(offset))))) =
          T.MOVE(T.TEMP(t1), T.MEM(T.BINOP(T.PLUS, T.CONST(offset), normalizeExp e)))
    *)
end

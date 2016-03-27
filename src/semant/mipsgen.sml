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
  fun opcode(op, immediate) = 
  let 
    val code = 
      case op of 
           PLUS => "add"
         | MINUS => "sub"
         | MUL => "mul"
         | DIV => "div"
         | AND => "and"
         | OR  => "or"
         | LSHIFT => "shl"
         | RSHIFT => "shr"
         | ARSHIFT => "shra"
         | XOR => "xor"
    fun suffix code = if immediate then code ^ "i" else code
  in 
    code ^ suffix code
  end 

  fun codegen frame tree = 
  let 
    val instList = ref (nil: A.instr list)

    fun emit instruction = instList := instruction :: !instList

    fun result gen = let val t = Temp.newtemp() in emit(gen t); t end

    fun munchStm(T.SEQ(first, rest)) = 
          (munchStm first; munchStm rest)

      | munchStm(T.LABEL(lab)) = 
          emit(A.LABEL{assem=label ^ ":\n", lab=lab})

      | munchStm(T.MOVE(T.TEMP(t1), T.MEM(T.BINOP(T.PLUS, T.CONST(offset), e)))) = 
          emit(A.OPER {assem="lw `d0, " ^ Int.toString offset ^ "(`s0`)\n",
                       dst=[t1], src=[munchExp e], jump=NONE})

      | munchStm(T.MOVE(T.TEMP(_) as t, T.MEM(T.BINOP(T.PLUS, e, T.CONST(_) as offset)))) = 
          munchStm(T.MOVE(t, T.MEM(T.BINOP(T.PLUS, offset, e)))) 

      | munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST(offset), e), T.TEMP(t)))) = 
          emit(A.OPER {assem="sw `s0, " ^ Int.toString offset ^ "(`d0`)\n",
                       dst=[munchExp e], src=[t], i
                       jump=NONE})

      | munchStm(T.MOVE(T.TEMP(_) as d, T.MEM(e))) = 
          munchStm(T.MOVE(d, T.MEM(T.BINOP(T.PLUS, T.CONST(0), e))))

      | munchStm(T.MOVE(T.MEM(e), T.TEMP(_) as d)) = 
          munchStm(T.MOVE(T.MEM(T.BINOP(T.PLUS, T.CONST(0), e)), d))

      | munchStm(T.MOVE(e1, e2)) = 
          emit(A.MOVE{assem="move `d0, `s0\n", dst: munchExp e1, src: munchExp e2})

      (* Not sure about matching the name in the pattern. In the book, they
      * store the result in a temp and load it out of that, but I'm not sure
      * if that's the best strategy for mips. p. 204 *)
      | munchStm(T.EXP(T.CALL(T.NAME(lab), args))) = 
          emit(A.OPER {assem="jal `j0\n",
                       src=munchArgs(0, args), 
                       (* TODO: calldefs- a list of registers trashed by function 
                       * calls (e.g. $ra). Explanation on page 204-205 *)
                       dst=calldefs, 
                       jump=SOME([lab])})

    and
    fun munchExp(T.BINOP(T.PLUS, e, T.CONST(a))) =
          munchExp(T.BINOP(T.PLUS, T.CONST(a), e))

      | munchExp(T.BINOP(op, e1, e2))
          munchBinop(op, e1, e2)

      | munchExp(T.MEM(e)) = 
          result(fn temp =>
            A.OPER {assem="lw `d0, 0(`s0)\n", 
                    dst=[temp], src=[munchExp e],
                    jump=NONE})

      | munchExp(T.TEMP(t)) = t

      | munchExp(T.NAME(lab)) = 
          result(fn temp =>
            A.LABEL {assem=Symbol.name lab, lab=lab})
            
      (* ESEQS will be removed by canonicalization *)
      | munchExp(T.CONST(e)) = 
          result(fn temp =>
            A.OPER {assem="addi `d0, "^ Int.toString e ^", 0\n", 
                    dst=[temp], src=[],
                    jump=NONE})

    and
    fun munchBinop(op, e, T.CONST(c)) =
          result(fn temp => 
            A.OPER {assem=opcode(op, true) ^ " `d0, `s0, " ^ Int.toString c ^ "\n",
                    dst=[temp], src=[munchExp e],
                    jump=NONE})
      | munchBinop(op, e1, e2) =
          result(fn temp =>
            A.OPER {assem=opcode(op, false) ^ " `d0, `s0, `s1\n",
                    dst=[temp], src=[munchExp e1, munchExp e2],
                    jump=NONE})

    and 
    (* TODO: this code creates registers for all the arguments to a function
    * call (for liveness analysis) and as a side effect, loads the code
    * according to the calling conventions. At this point, it assumes infinite
    * argument registers and pushes nothing onto the stack *)
    fun munchArgs(i, fst::rest) = 
      result(fn temp =>
        A. OPER {assem="move $a" ^ Int.toString i ^ ", `s0\n",
                 dst=[temp], src=[munchExp fst],
                 jump=NONE})::munchArgs(i + 1, rest)
      | munchArgs(i, []) = []

    in
        munchStm tree;
        rev !instList
    (*
    * fun normalizeStm(T.MOVE(T.TEMP(t1), T.MEM(T.BINOP(T.PLUS, e, T.CONST(offset))))) = 
          T.MOVE(T.TEMP(t1), T.MEM(T.BINOP(T.PLUS, T.CONST(offset), normalizeExp e)))
    *)
end
  

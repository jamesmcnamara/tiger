structure MipsFrame : FRAME =
struct

  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name: Temp.label, formals: access list, offset: int ref}
  type register = string

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val wordSize = 4
  val sizeOfK = 10 (* THIS NEEDS TO BE UPDATED! Im not sure what the actual value is *)
  val FP = Temp.newtemp()
  val RV = Temp.newtemp()
  val RA = Temp.newtemp()
  val SP = Temp.newtemp()
  val ZERO = Temp.newtemp()
  val cs1 = Temp.newtemp()
  val cs2 = Temp.newtemp()
  val cs3 = Temp.newtemp()
  val cs4 = Temp.newtemp()
  val cs5 = Temp.newtemp()
  val cs6 = Temp.newtemp()
  val cs7 = Temp.newtemp()
  val cs8 = Temp.newtemp()
  val arg1 = Temp.newtemp()
  val arg2 = Temp.newtemp()
  val arg3 = Temp.newtemp()
  val arg4 = Temp.newtemp()

  val argtemps = [arg1, arg2, arg3, arg4]
  val argregs = ["$a0", "$a1", "$a2", "$a3"]
  val calleesavestemps = [cs1,cs2,cs3,cs4,cs5,cs6,cs7,cs8]
  val argregs = ["$a0", "$a1", "$a2", "$a3"]
  val calleesaves = ["$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"]
  val callersaves = ["$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
  "$t8", "$t9"]
  val registers = ["$v0", "$ra", "$sp", "$zero", "$fp"] @ calleesaves @ argregs

  val availableRegisters = callersaves

  fun precolor() : register Temp.Table.table  =
    let val specials = [RV,RA,SP,ZERO,FP] @ calleesavestemps @ argtemps
        val pairs = (ListPair.map (fn (reg,temp) => (reg,temp)) (registers, specials))
    in
      foldl (fn((reg,temp),table) => Temp.Table.enter(table,temp,reg))
            Temp.Table.empty
            pairs
    end

  val tempMap = precolor()

  fun getAccess(formal, (formals,offset)) =
    if formal then (InFrame(offset)::formals,offset-wordSize)
    else (InReg(Temp.newtemp())::formals,offset)

  fun newFrame {name, formals} =
    let val (formals',offset) = foldl getAccess ([],0) formals
    in
      {name=name, formals=formals', offset=(ref offset)}
    end

  fun formals({name,formals,offset}) = formals
  fun name({name,formals,offset}) = name
  fun offset({name,formals,offset}) = !offset

  fun allocLocal {name,formals,offset} true =
      let val access = InFrame(!offset)
      in
        (offset := !offset-wordSize;
         access)
      end
    | allocLocal _ false = InReg(Temp.newtemp())

  fun externalCall(s,args) = Tree.CALL(Tree.NAME(Temp.namedlabel(s)), args)


  (* For testing *)
  (* TODO: This will need to be expanded for RA... *)
  val registersAsTemps = [FP]

  fun accessToExp(access) =
    case access of
      InReg(t) => Tree.TEMP(t)
    | InFrame(offset) => Tree.MEM(Tree.BINOP(Tree.PLUS,Tree.TEMP(FP), Tree.CONST(offset)))

  fun seq [] = Tree.EXP(Tree.CONST(0))
    | seq [stm] = stm
    | seq(stm::stms) = Tree.SEQ(stm, seq(stms))

  fun move(reg,offset) = Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.MINUS,Tree.TEMP(FP),Tree.CONST(offset))),reg)

  fun saveRegs ([], offset) = Tree.EXP(Tree.CONST(0))
    | saveRegs ([reg], offset) = move(reg,offset)
    | saveRegs (reg::regs, offset) = Tree.SEQ(move(reg,offset),saveRegs(regs,offset+1))

  fun procEntryExit1({name, formals, offset}, body) =
    (*
      Move args -> moveArgsStm
      Move registers -> savedRegsStm
      body
      restore registers
    *)
    let val args = foldl (fn(formal,acc) => accessToExp(formal)::acc) [] formals
        val moveArgs = ListPair.map (fn(reg,arg) => Tree.MOVE(Tree.TEMP(reg),arg)) (argtemps, args)
        val moveArgsStm = seq(moveArgs)
        val regsToSave = RV::calleesavestemps
        val toSaveAlloc = map (fn(a) => allocLocal {name=name,formals=formals,offset=offset} true) regsToSave
        val savedRegsStm = ListPair.map (fn(reg,alloc) => Tree.MOVE(accessToExp(alloc),Tree.TEMP(reg))) (regsToSave,toSaveAlloc)
        val restoreRegsStm = ListPair.map (fn(reg,alloc) => Tree.MOVE(Tree.TEMP(reg), accessToExp(alloc))) (regsToSave,toSaveAlloc)
    in
      seq([moveArgsStm, seq(savedRegsStm), body, seq(restoreRegsStm)])
    end

  fun procEntryExit2(frame,body) =
    body @
    [Assem.OPER{assem="",
            src=[ZERO,RA,SP]@calleesavestemps,
            dst=[],
            jump=SOME[]}]

  fun procEntryExit3(frame, instrs) =
    let val frameSize = offset(frame) - (5 * wordSize)
        val prolog = Symbol.name(name(frame)) ^ ":\n" ^
                     "sub $sp, $sp, " ^ Int.toString(frameSize) ^ "\n"
        val epilog = "addi $sp, $sp, " ^ Int.toString(frameSize) ^ "\n" ^
                     "jr $ra\n"
    in
      {prolog=prolog,body=instrs,epilog=epilog}
    end

  fun string(label,value) = Symbol.name(label) ^ ": .ascii \"" ^ value ^ "\"\n"

end

structure Frame : FRAME = MipsFrame

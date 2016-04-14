structure MipsFrame : FRAME =
struct
 
  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name: Temp.label, formals: access list, offset: int ref}
  type register = string

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val wordSize = 4
  val FP = Temp.newtemp()
  val RV = Temp.newtemp()
  val RA = Temp.newtemp()
  val SP = Temp.newtemp()
  val ZERO = Temp.newtemp()

  val specialregs = ["$r0", "$ra", "$sp", "$zero", "$fp", "$k0", "$k1"]
  val argregs = ["$a0", "$a1", "$a2", "$a3"] 
  val calleesaves = ["$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"]
  val callersaves = ["$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7",
  "$t8", "$t9"]

  fun precolor() : register Temp.Table.table  = 
    let 
      val specials = [RV, RA, SP, ZERO, FP]
      val enter = Temp.Table.enter
      fun fold (table, temp::temps, reg::regs)= 
        fold (enter (table, temp, reg), temps, regs)
      fun fold (table, _, _) =
        table
    in
      fold(Temp.Table.empty, specials, specialregs)
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

end

structure Frame : FRAME = MipsFrame

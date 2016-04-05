structure MipsFrame : FRAME =
struct

  datatype access = InFrame of int | InReg of Temp.temp
  type frame = {name: Temp.label, formals: access list, offset: int ref}

  datatype frag = PROC of {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val wordSize = 4
  val FP = Temp.newtemp()
  val RA = Temp.newtemp()
  val RV = Temp.newtemp()

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

end

structure Frame : FRAME = MipsFrame

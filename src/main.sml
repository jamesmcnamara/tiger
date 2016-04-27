signature MAIN =
sig
    val compile : string -> unit
end

structure Main : MAIN =
struct
exception NotImplemented

fun compile filename =
  let
    fun toAsm(Frame.PROC {body, frame}) =
        let
          (* Canon *)
          val stms = Canon.linearize body
          val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
          val assem = List.concat(map (CodeGen.codegen frame) stms')
          val assem' = Frame.procEntryExit2(frame,assem)
          val { prolog, epilog, body=assem''} = Frame.procEntryExit3(frame, assem')
          val (assem''', alloc) = RegAlloc.alloc(assem'', frame)
          val replace = (fn(temp) => valOf(Temp.Table.look(alloc,temp)))
          val replaceAll = map (fn(instr) => Assem.format replace instr) assem'''
          val replaced = foldr (fn(a,b) => a ^ b) "" replaceAll
        in
          ("", (prolog ^ replaced ^ epilog))
        end
      | toAsm(Frame.STRING (label, value)) =
        (Frame.string(label,value),"")

    val exp = Parse.parse(filename)
    val _ =  FindEscape.findEscape(exp)
    val ir = Semant.transProg(exp)
    val asm = map toAsm ir
    val (finalString,finalProc) = foldr (fn((string,proc),(stringAcc,procAcc)) => (string ^ stringAcc, proc ^ procAcc)) ("","") asm
    val final = ".text\n.globl main\n" ^ finalProc ^ ".data\n" ^ finalString
    val out = TextIO.openOut "out.asm"
  in
    TextIO.output(out, final);
    TextIO.closeOut(out)
  end
end

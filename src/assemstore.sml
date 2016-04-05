(* save and load Assem lists to ML files *)
(* NOTE: to load saved data, you must implement:
     Temp.tempint
     MipsFrame.registersAsTemps

maybe other things....  Once everything is in place, you can
just "use hello.sml" to get some assembly code to play with.
     *)
structure AssemStore =
struct

(* temps that should come back with the same value they were saved as;
   other temps merely come back internally consistent *)
val fixedTemps = MipsFrame.registersAsTemps


(* the way temp's and label's will be stored in the file: as integers *)
type tempkey = int
val storeTempkey = Int.toString

type labelkey = string
fun storeLabelkey(l) = "\"" ^ (String.toString l) ^ "\""


(* a nicer table than the one given by the TABLE sig *)
structure TempTable = BinaryMapFn(struct type ord_key=Temp.temp
					 val compare =
					     (fn (t1,t2) =>
						 Int.compare(Temp.tempint t1,
							     Temp.tempint t2))
				  end)

structure IntTable = BinaryMapFn(struct type ord_key=int val compare=Int.compare end)

(* an assembly operation with temp's replaced by int's *) 
datatype insr =
         OPER of {assem:string, src:tempkey list, dst:tempkey list, jump: labelkey list option} |
         MOVE of {assem:string, src:tempkey, dst: tempkey} |
         LABEL of {assem:string, lab:labelkey}

(* ... encode functions removed  ... *)

fun decode (name, stackslots_used, insrs)  =
    let
	(* make a table from int's back to temp's *)
	fun tempKeysIn(OPER{assem,src,dst,jump}) =
	    src @ dst
	  | tempKeysIn(MOVE{assem,src,dst})  =
	    [src, dst]
	  | tempKeysIn(LABEL _) = []

	val allTempKeys = foldr op @ [] (map tempKeysIn insrs)

	val baseTempTable = foldl (fn (temp, table) =>
				      IntTable.insert(table,
						      IntTable.numItems table,
						      temp))
				  IntTable.empty
				  fixedTemps

	val tempTable = foldl (fn (tempkey, table) =>
				  case IntTable.find(table, tempkey) of
				      NONE => 
				      (* not in table -- add a temp *)
				      IntTable.insert(table,
						       tempkey,
						       Temp.newtemp())
				    | SOME _ =>
				      (* already in table *)
				      table)
			      baseTempTable
			      allTempKeys

	fun decodeTemp key = valOf(IntTable.find(tempTable, key))

	fun decodeInsr(OPER{assem,src,dst,jump}) =
	    Assem.OPER{assem=assem,
		       src=map decodeTemp src,
		       dst=map decodeTemp dst,
		       jump=Option.map (map Symbol.symbol) jump}
	  | decodeInsr(MOVE{assem, src, dst}) =
	    Assem.MOVE{assem=assem, src=decodeTemp src, dst=decodeTemp dst}
	  | decodeInsr(LABEL{assem, lab}) =
	    Assem.LABEL{assem=assem, lab=Symbol.symbol lab}
    in (Symbol.symbol name, stackslots_used, map decodeInsr insrs)
    end

end

structure Types = struct

type unique = unit ref

datatype ty = RECORD of (Symbol.symbol * ty) list * unique
            | UNIT
            | NIL
            | INT
            | STRING
            | ARRAY of ty * unique
	| NAME of Symbol.symbol * ty option ref
(*
fun toString(RECORD(p:ps, _) =
            | UNIT
            | NIL
            | INT
            | STRING
            | ARRAY of ty * unique
	| NAME of Symbol.symbol * ty option ref
*)

end

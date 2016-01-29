signature SRC_STRING =
sig
    val new : int -> unit
    val push : string * int -> unit
    val pushString : string -> unit
    val pushAscii : string -> unit
    val pushControl : string -> unit
    val emit : int -> Tokens.token
end

structure SrcString :> SRC_STRING = struct
    val startPos = ref 0
    val innerString = ref ""
    val innerLength = ref 0

    fun new start =
        (startPos := start;
         innerString := "";
         innerLength := 0)

    fun push (str, len) =
        (innerString := !innerString ^ str;
         innerLength := !innerLength + len)

    fun pushString (str) =
        push(str, size(str))

    fun pushAscii (numStr) =
        let val num = valOf (Int.fromString numStr)
            val str = String.str(chr num)
        in
            if num > 255 then
                ErrorMsg.error("illegal ascii value: " ^ numStr)
            else
                push(str, 4)
        end

    fun pushControl (text) =
        case explode text of
            [#"^", c] =>
                let val ascii = (ord c) - 64 in
                if ascii < 0 orelse ascii > 31 then
                    ErrorMsg.error("illegal control sequence: " ^ Int.toString ascii)
                else
                    push(String.str(chr ascii), 3)
                end
            | err =>
                ErrorMsg.error("unrecognized control sequence: " ^ text)

    fun emit (yypos) =
        Tokens.STRING(!innerString, !startPos, yypos)

end

structure SrcString :> SRC_STRING = struct
    val startPos = ref 0
    val innerString = ref ""
    val innerLength = ref 0

    type yypos = int

    fun push (str, len) =
        (innerString := !innerString ^ str;
         innerLength := !innerLength + len)

    fun new yypos =
        (startPos := yypos;
         innerString := "";
         innerLength := 0)

    fun pushString (str, yypos) =
        push(str, size(str))

    fun pushAscii (numStr, yypos) =
        let val num = valOf (Int.fromString numStr)
            val str = String.str(chr num)
        in
            if num > 255 then
                ErrorMsg.error yypos ("illegal ascii value: " ^ numStr)
            else
                push(str, 4)
        end

    fun pushControl (text, yypos) =
        case explode text of
            [#"^", c] =>
                let val ascii = (ord c) - 64 in
                if ascii < 0 orelse ascii > 31 then
                    ErrorMsg.error yypos ("illegal control sequence: " ^ Int.toString ascii)
                else
                    push(String.str(chr ascii), 3)
                end
            | err =>
                ErrorMsg.error yypos ("unrecognized control sequence: " ^ text)

    fun emit (yypos) =
        Tokens.STRING(!innerString, !startPos, yypos)
end

structure SrcString = struct
    val inner = ref ""
    val start = ref 0
    val endPos = ref 0

    fun new start_pos =
        (inner := "";
        start := start_pos;
        endPos := start_pos)

    fun push (str, len) =
        (inner := !inner ^ str;
         endPos := !endPos + len)

    fun pushString (str) =
        (inner := !inner ^ str;
         endPos := !endPos + size(str))

    fun pushAscii (text, yypos)  =
        let
            val num = valOf (Int.fromString text)
        in
            if num > 255 then
                ErrorMsg.error yypos ("illegal escape sequence" ^ text)
            else
                push (Char.toString (chr num), yypos)
        end

    fun pushControl (text, yypos) =
        case explode text of
            [#"^", c] =>
                let val ascii = (ord c) - 64 in
                if ascii < 0 orelse ascii > 31 then
                    ErrorMsg.error yypos ("illegal control sequence" ^ Int.toString ascii)
                else
                    pushString (Char.toString (chr ascii))
                end
            | err =>
                ErrorMsg.error yypos ("unrecognized control sequence" ^ text)

    fun emit () =
        Tokens.STRING(!inner, !start, !endPos + 2, !Tracking.lineNum)

end

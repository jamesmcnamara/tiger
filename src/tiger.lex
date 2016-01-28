type pos = int
type lexresult = Tokens.token

val lineNum = Tracking.lineNum
val linePos = Tracking.linePos
val currentLineStartPos = Tracking.currentLineStartPos
val commentDepth = ref 0;
fun err(p1,p2) = ErrorMsg.error p1

fun eof() =
    let val pos = Tracking.getLinePos(hd(!linePos)) in
        let val eofToken = Tokens.EOF(pos,pos,!lineNum) in
            (ErrorMsg.reset();
             Tracking.reset();
             eofToken)
        end
    end

fun atoi(a) =
    valOf (Int.fromString a)

fun updateDepth amt =
    commentDepth := !commentDepth + amt

fun incComment() =
    updateDepth 1

fun decComment() =
    updateDepth ~1

structure SrcString = struct
    val inner = ref ""
    val start = ref 0

    fun new start_pos =
        (inner := "";
        start := start_pos)

    fun push (str, yypos) =
        (inner := !inner ^ str)

    fun pushAscii (text, yypos)  =
        let
            val num = atoi text
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
                    push (Char.toString (chr ascii), yypos)
                end
            | err =>
                ErrorMsg.error yypos ("unrecognized control sequence" ^ text)

    fun emit () =
        Tokens.STRING(!inner, !start, !start + size (!inner), !lineNum)

end

%%
%s ESCAPE COMMENT STRING;

digit = [0-9];
id = [a-zA-Z][a-zA-Z0-9_]*;
alpha = [a-zA-Z];
whitespace = [\n\t\r ];

%%
<INITIAL>type     => (Tokens.TYPE(Tracking.getLinePos(yypos), size yytext,!lineNum));
<INITIAL>var      => (Tokens.VAR(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>function => (Tokens.FUNCTION(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>break    => (Tokens.BREAK(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>of       => (Tokens.OF(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>end      => (Tokens.END(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>in       => (Tokens.IN(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>nil      => (Tokens.NIL(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>let      => (Tokens.LET(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>do       => (Tokens.DO(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>to       => (Tokens.TO(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>for      => (Tokens.FOR(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>while    => (Tokens.WHILE(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>else     => (Tokens.ELSE(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>then     => (Tokens.THEN(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>if       => (Tokens.IF(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>array    => (Tokens.ARRAY(Tracking.getLinePos(yypos), size yytext, !lineNum));

<INITIAL>:\=  => (Tokens.ASSIGN(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\|   => (Tokens.OR(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>&    => (Tokens.AND(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\>\= => (Tokens.GE(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\>   => (Tokens.GT(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\<\= => (Tokens.LE(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\<   => (Tokens.LT(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\<\> => (Tokens.NEQ(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\=   => (Tokens.EQ(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\/   => (Tokens.DIVIDE(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\*   => (Tokens.TIMES(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>-    => (Tokens.MINUS(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\+   => (Tokens.PLUS(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\.   => (Tokens.DOT(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\}   => (Tokens.RBRACE(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\{   => (Tokens.LBRACE(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\]   => (Tokens.RBRACK(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>"\[" => (Tokens.LBRACK(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\)   => (Tokens.RPAREN(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\(   => (Tokens.LPAREN(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\;   => (Tokens.SEMICOLON(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>:    => (Tokens.COLON(Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>,    => (Tokens.COMMA(Tracking.getLinePos(yypos), size yytext, !lineNum));

<INITIAL>{digit}+  => (Tokens.INT(atoi yytext, Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>{id}     => (Tokens.ID(yytext, Tracking.getLinePos(yypos), size yytext, !lineNum));
<INITIAL>[ \t]*   => (continue());

<INITIAL>\"   => (YYBEGIN STRING; SrcString.new(Tracking.getLinePos(yypos)); continue());
<STRING>\"    => (YYBEGIN INITIAL; SrcString.emit());
<STRING>\\  => (YYBEGIN ESCAPE; continue());
<ESCAPE>n => (SrcString.push("\n", Tracking.getLinePos(yypos)); YYBEGIN STRING; continue());
<ESCAPE>t => (SrcString.push("\t", Tracking.getLinePos(yypos)); YYBEGIN STRING; continue());
<ESCAPE>\\ => (SrcString.push("\\", Tracking.getLinePos(yypos)); YYBEGIN STRING; continue());
<ESCAPE>{digit}{3}  => (SrcString.pushAscii(yytext, Tracking.getLinePos(yypos));
                            YYBEGIN STRING;  continue());
<ESCAPE>\^.  => (SrcString.pushControl(yytext, Tracking.getLinePos(yypos));
                    YYBEGIN STRING; continue());
<ESCAPE>{whitespace}*\\  => (YYBEGIN STRING; continue());
<STRING>.     => (SrcString.push(yytext, Tracking.getLinePos(yypos)); continue());


<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; incComment(); continue());
<COMMENT>"*/"         => (decComment(); if !commentDepth=0 then YYBEGIN INITIAL else (); continue());
<COMMENT>.            => (continue());

"\n" => (Tracking.incrementLine(yypos); continue());

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

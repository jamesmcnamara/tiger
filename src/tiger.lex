type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0;
fun err(p1,p2) = ErrorMsg.error p1

val currentLineStartPos = ref 1

fun getLinePos yypos = (yypos - !currentLineStartPos)

fun eof() =
    let val pos = getLinePos(hd(!linePos)) in
        let val eofToken = Tokens.EOF(pos,pos,!lineNum) in
            (ErrorMsg.reset();
                eofToken)
        end
    end

fun atoi(a) =
    valOf (Int.fromString a)

fun handle_newline(continue, yypos, yytext) =
  (linePos := yypos :: !linePos;
  currentLineStartPos := yypos;
  ErrorMsg.incrementLine();
  continue());

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
<INITIAL>type     => (Tokens.TYPE(getLinePos(yypos), size yytext,!lineNum));
<INITIAL>var      => (Tokens.VAR(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>function => (Tokens.FUNCTION(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>break    => (Tokens.BREAK(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>of       => (Tokens.OF(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>end      => (Tokens.END(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>in       => (Tokens.IN(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>nil      => (Tokens.NIL(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>let      => (Tokens.LET(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>do       => (Tokens.DO(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>to       => (Tokens.TO(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>for      => (Tokens.FOR(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>while    => (Tokens.WHILE(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>else     => (Tokens.ELSE(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>then     => (Tokens.THEN(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>if       => (Tokens.IF(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>array    => (Tokens.ARRAY(getLinePos(yypos), size yytext, !lineNum));

<INITIAL>:\=  => (Tokens.ASSIGN(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\|   => (Tokens.OR(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>&    => (Tokens.AND(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\>\= => (Tokens.GE(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\>   => (Tokens.GT(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\<\= => (Tokens.LE(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\<   => (Tokens.LT(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\<\> => (Tokens.NEQ(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\=   => (Tokens.EQ(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\/   => (Tokens.DIVIDE(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\*   => (Tokens.TIMES(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>-    => (Tokens.MINUS(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\+   => (Tokens.PLUS(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\.   => (Tokens.DOT(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\}   => (Tokens.RBRACE(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\{   => (Tokens.LBRACE(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\]   => (Tokens.RBRACK(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>"\[" => (Tokens.LBRACK(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\)   => (Tokens.RPAREN(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\(   => (Tokens.LPAREN(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>\;   => (Tokens.SEMICOLON(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>:    => (Tokens.COLON(getLinePos(yypos), size yytext, !lineNum));
<INITIAL>,    => (Tokens.COMMA(getLinePos(yypos), size yytext, !lineNum));

<INITIAL>{digit}+  => (Tokens.INT(atoi yytext, getLinePos(yypos), getLinePos(yypos) + size yytext, !lineNum));
<INITIAL>{id}     => (Tokens.ID(yytext, getLinePos(yypos), size yytext, !lineNum));
<INITIAL>[ \t]*   => (continue());

<INITIAL>\"   => (YYBEGIN STRING; SrcString.new(getLinePos(yypos)); continue());
<STRING>\"    => (YYBEGIN INITIAL; SrcString.emit());
<STRING>\\  => (YYBEGIN ESCAPE; continue());
<ESCAPE>n => (SrcString.push("\n", getLinePos(yypos)); YYBEGIN STRING; continue());
<ESCAPE>t => (SrcString.push("\t", getLinePos(yypos)); YYBEGIN STRING; continue());
<ESCAPE>\\ => (SrcString.push("\\", getLinePos(yypos)); YYBEGIN STRING; continue());
<ESCAPE>{digit}{3}  => (SrcString.pushAscii(yytext, getLinePos(yypos));
                            YYBEGIN STRING;  continue());
<ESCAPE>\^.  => (SrcString.pushControl(yytext, getLinePos(yypos));
                    YYBEGIN STRING; continue());
<ESCAPE>{whitespace}*\\  => (YYBEGIN STRING; continue());
<STRING>.     => (SrcString.push(yytext, getLinePos(yypos)); continue());


<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; incComment(); continue());
<COMMENT>"*/"         => (decComment(); if !commentDepth=0 then YYBEGIN INITIAL else (); continue());
<COMMENT>.            => (continue());

"\n" => (handle_newline(continue, yypos, yytext));

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

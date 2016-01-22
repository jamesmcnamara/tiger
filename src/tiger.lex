type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentDepth = ref 0;
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

fun atoi(a) =
    valOf (Int.fromString a)

fun handle_newline(continue, yypos, yytext) =
  (lineNum := !lineNum+1;
  linePos := yypos :: !linePos;
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
        Tokens.STRING(!inner, !start, !start + size (!inner))
end

%% 
%s ESCAPE COMMENT STRING;

digit = [0-9];
id = [a-zA-Z][a-zA-Z0-9_]*;
alpha = [a-zA-Z];
whitespace = [\n\t\r ];

%%
<INITIAL>type     => (Tokens.TYPE(yypos, size yytext));
<INITIAL>var      => (Tokens.VAR(yypos, size yytext));
<INITIAL>function => (Tokens.FUNCTION(yypos, size yytext));
<INITIAL>break    => (Tokens.BREAK(yypos, size yytext));
<INITIAL>of       => (Tokens.OF(yypos, size yytext));
<INITIAL>end      => (Tokens.END(yypos, size yytext));
<INITIAL>in       => (Tokens.IN(yypos, size yytext));
<INITIAL>nil      => (Tokens.NIL(yypos, size yytext));
<INITIAL>let      => (Tokens.LET(yypos, size yytext));
<INITIAL>do       => (Tokens.DO(yypos, size yytext));
<INITIAL>to       => (Tokens.TO(yypos, size yytext));
<INITIAL>for      => (Tokens.FOR(yypos, size yytext));
<INITIAL>while    => (Tokens.WHILE(yypos, size yytext));
<INITIAL>else     => (Tokens.ELSE(yypos, size yytext));
<INITIAL>then     => (Tokens.THEN(yypos, size yytext));
<INITIAL>if       => (Tokens.IF(yypos, size yytext));
<INITIAL>array    => (Tokens.ARRAY(yypos, size yytext));

<INITIAL>:\=  => (Tokens.ASSIGN(yypos, size yytext));
<INITIAL>\|   => (Tokens.OR(yypos, size yytext));
<INITIAL>&    => (Tokens.AND(yypos, size yytext));
<INITIAL>\>\= => (Tokens.GE(yypos, size yytext));
<INITIAL>\>   => (Tokens.GT(yypos, size yytext));
<INITIAL>\<\= => (Tokens.LE(yypos, size yytext));
<INITIAL>\<   => (Tokens.LT(yypos, size yytext));
<INITIAL>\<\> => (Tokens.NEQ(yypos, size yytext));
<INITIAL>\=   => (Tokens.EQ(yypos, size yytext));
<INITIAL>\/   => (Tokens.DIVIDE(yypos, size yytext));
<INITIAL>\*   => (Tokens.TIMES(yypos, size yytext));
<INITIAL>-    => (Tokens.MINUS(yypos, size yytext));
<INITIAL>\+   => (Tokens.PLUS(yypos, size yytext));
<INITIAL>\.   => (Tokens.DOT(yypos, size yytext));
<INITIAL>\}   => (Tokens.RBRACE(yypos, size yytext));
<INITIAL>\{   => (Tokens.LBRACE(yypos, size yytext));
<INITIAL>\]   => (Tokens.RBRACK(yypos, size yytext));
<INITIAL>"\[" => (Tokens.LBRACK(yypos, size yytext));
<INITIAL>\)   => (Tokens.RPAREN(yypos, size yytext));
<INITIAL>\(   => (Tokens.LPAREN(yypos, size yytext));
<INITIAL>\;   => (Tokens.SEMICOLON(yypos, size yytext));
<INITIAL>:    => (Tokens.COLON(yypos, size yytext));
<INITIAL>,    => (Tokens.COMMA(yypos, size yytext));

<INITIAL>{digit}+  => (Tokens.INT(atoi yytext, yypos, yypos + size yytext));
<INITIAL>{id}     => (Tokens.ID(yytext, yypos, size yytext));
<INITIAL>[ \t]*   => (continue());

<INITIAL>\"   => (YYBEGIN STRING; SrcString.new yypos; continue());
<STRING>\"    => (YYBEGIN INITIAL; SrcString.emit());
<STRING>\\  => (YYBEGIN ESCAPE; continue());
<ESCAPE>n => (SrcString.push("\n", yypos); YYBEGIN STRING; continue());
<ESCAPE>t => (SrcString.push("\t", yypos); YYBEGIN STRING; continue());
<ESCAPE>\\ => (SrcString.push("\\", yypos); YYBEGIN STRING; continue());
<ESCAPE>{digit}{3}  => (SrcString.pushAscii(yytext, yypos); 
                            YYBEGIN STRING;  continue());
<ESCAPE>\^.  => (SrcString.pushControl(yytext, yypos); 
                    YYBEGIN STRING; continue());
<ESCAPE>{whitespace}*\\  => (YYBEGIN STRING; continue());
<STRING>.     => (SrcString.push(yytext, yypos); continue());


<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; incComment(); continue());
<COMMENT>"*/"         => (decComment(); if !commentDepth=0 then YYBEGIN INITIAL else (); continue());
<COMMENT>.            => (continue());

"\n" => (handle_newline(continue, yypos, yytext));

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());


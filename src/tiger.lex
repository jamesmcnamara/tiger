type pos = int
type lexresult = Tokens.token

val commentDepth = ref 0;
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = Tokens.EOF

fun atoi(a) =
    valOf (Int.fromString a)

fun updateDepth amt =
    commentDepth := !commentDepth + amt

fun incComment() =
    updateDepth 1

fun decComment() =
    updateDepth ~1

%%

%s ESCAPE COMMENT STRING;
%posarg

digit = [0-9];
id = [a-zA-Z][a-zA-Z0-9_]*;
alpha = [a-zA-Z];
whitespace = [\n\t\r ];

%%
<INITIAL>type     => (Tokens.TYPE(yypos - 1, yypos + size yytext - 2));
<INITIAL>var      => (Tokens.VAR(yypos - 1, yypos + size yytext - 2));
<INITIAL>function => (Tokens.FUNCTION(yypos - 1, yypos + size yytext - 2));
<INITIAL>break    => (Tokens.BREAK(yypos - 1, yypos + size yytext - 2));
<INITIAL>of       => (Tokens.OF(yypos - 1, yypos + size yytext - 2));
<INITIAL>end      => (Tokens.END(yypos - 1, yypos + size yytext - 2));
<INITIAL>in       => (Tokens.IN(yypos - 1, yypos + size yytext - 2));
<INITIAL>nil      => (Tokens.NIL(yypos - 1, yypos + size yytext - 2));
<INITIAL>let      => (Tokens.LET(yypos - 1, yypos + size yytext - 2));
<INITIAL>do       => (Tokens.DO(yypos - 1, yypos + size yytext - 2));
<INITIAL>to       => (Tokens.TO(yypos - 1, yypos + size yytext - 2));
<INITIAL>for      => (Tokens.FOR(yypos - 1, yypos + size yytext - 2));
<INITIAL>while    => (Tokens.WHILE(yypos - 1, yypos + size yytext - 2));
<INITIAL>else     => (Tokens.ELSE(yypos - 1, yypos + size yytext - 2));
<INITIAL>then     => (Tokens.THEN(yypos - 1, yypos + size yytext - 2));
<INITIAL>if       => (Tokens.IF(yypos - 1, yypos + size yytext - 2));
<INITIAL>array    => (Tokens.ARRAY(yypos - 1, yypos + size yytext - 2));

<INITIAL>:\=  => (Tokens.ASSIGN(yypos - 1, yypos + size yytext - 2));
<INITIAL>\|   => (Tokens.OR(yypos - 1, yypos + size yytext - 2));
<INITIAL>&    => (Tokens.AND(yypos - 1, yypos + size yytext - 2));
<INITIAL>\>\= => (Tokens.GE(yypos - 1, yypos + size yytext - 2));
<INITIAL>\>   => (Tokens.GT(yypos - 1, yypos + size yytext - 2));
<INITIAL>\<\= => (Tokens.LE(yypos - 1, yypos + size yytext - 2));
<INITIAL>\<   => (Tokens.LT(yypos - 1, yypos + size yytext - 2));
<INITIAL>\<\> => (Tokens.NEQ(yypos - 1, yypos + size yytext - 2));
<INITIAL>\=   => (Tokens.EQ(yypos - 1, yypos + size yytext - 2));
<INITIAL>\/   => (Tokens.DIVIDE(yypos - 1, yypos + size yytext - 2));
<INITIAL>\*   => (Tokens.TIMES(yypos - 1, yypos + size yytext - 2));
<INITIAL>-    => (Tokens.MINUS(yypos - 1, yypos + size yytext - 2));
<INITIAL>\+   => (Tokens.PLUS(yypos - 1, yypos + size yytext - 2));
<INITIAL>\.   => (Tokens.DOT(yypos - 1, yypos + size yytext - 2));
<INITIAL>\}   => (Tokens.RBRACE(yypos - 1, yypos + size yytext - 2));
<INITIAL>\{   => (Tokens.LBRACE(yypos - 1, yypos + size yytext - 2));
<INITIAL>\]   => (Tokens.RBRACK(yypos - 1, yypos + size yytext - 2));
<INITIAL>"\[" => (Tokens.LBRACK(yypos - 1, yypos + size yytext - 2));
<INITIAL>\)   => (Tokens.RPAREN(yypos - 1, yypos + size yytext - 2));
<INITIAL>\(   => (Tokens.LPAREN(yypos - 1, yypos + size yytext - 2));
<INITIAL>\;   => (Tokens.SEMICOLON(yypos - 1, yypos + size yytext - 2));
<INITIAL>:    => (Tokens.COLON(yypos - 1, yypos + size yytext - 2));
<INITIAL>,    => (Tokens.COMMA(yypos - 1, yypos + size yytext - 2));

<INITIAL>{digit}+  => (Tokens.INT(atoi yytext, yypos, yypos + size yytext - 2));
<INITIAL>{id}     => (Tokens.ID(yytext, yypos, yypos + size yytext - 2));
<INITIAL>[ \t]*   => (continue());

<INITIAL>\"   => (YYBEGIN STRING; SrcString.new(yypos); continue());
<STRING>\"    => (YYBEGIN INITIAL; SrcString.emit(yypos));
<STRING>\\  => (YYBEGIN ESCAPE; continue());
<ESCAPE>n => (SrcString.pushString("\n", yypos); YYBEGIN STRING; continue());
<ESCAPE>t => (SrcString.pushString("\t", yypos); YYBEGIN STRING; continue());
<ESCAPE>\\ => (SrcString.pushString("\\", yypos); YYBEGIN STRING; continue());
<ESCAPE>{digit}{3}  => (SrcString.pushAscii(yytext, yypos);
                            YYBEGIN STRING;  continue());
<ESCAPE>\^.  => (SrcString.pushControl(yytext, yypos);
                    YYBEGIN STRING; continue());
<ESCAPE>{whitespace}*\\  => (YYBEGIN STRING; continue());
<STRING>.     => (SrcString.pushString(yytext, yypos); continue());


<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; incComment(); continue());
<COMMENT>"*/" => (decComment(); if !commentDepth=0 then YYBEGIN INITIAL else (); continue());
<COMMENT>"\n" => (Newline.add(yypos); continue());
<COMMENT>. => (continue());

"\n" => (Newline.add(yypos); continue());

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

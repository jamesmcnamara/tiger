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

digit = [0-9];
id = [a-zA-Z][a-zA-Z0-9_]*;
alpha = [a-zA-Z];
whitespace = [\n\t\r ];

%%
<INITIAL>type     => (Tokens.TYPE(yypos, yypos + size yytext));
<INITIAL>var      => (Tokens.VAR(yypos, yypos + size yytext));
<INITIAL>function => (Tokens.FUNCTION(yypos, yypos + size yytext));
<INITIAL>break    => (Tokens.BREAK(yypos, yypos + size yytext));
<INITIAL>of       => (Tokens.OF(yypos, yypos + size yytext));
<INITIAL>end      => (Tokens.END(yypos, yypos + size yytext));
<INITIAL>in       => (Tokens.IN(yypos, yypos + size yytext));
<INITIAL>nil      => (Tokens.NIL(yypos, yypos + size yytext));
<INITIAL>let      => (Tokens.LET(yypos, yypos + size yytext));
<INITIAL>do       => (Tokens.DO(yypos, yypos + size yytext));
<INITIAL>to       => (Tokens.TO(yypos, yypos + size yytext));
<INITIAL>for      => (Tokens.FOR(yypos, yypos + size yytext));
<INITIAL>while    => (Tokens.WHILE(yypos, yypos + size yytext));
<INITIAL>else     => (Tokens.ELSE(yypos, yypos + size yytext));
<INITIAL>then     => (Tokens.THEN(yypos, yypos + size yytext));
<INITIAL>if       => (Tokens.IF(yypos, yypos + size yytext));
<INITIAL>array    => (Tokens.ARRAY(yypos, yypos + size yytext));

<INITIAL>:\=  => (Tokens.ASSIGN(yypos, yypos + size yytext));
<INITIAL>\|   => (Tokens.OR(yypos, yypos + size yytext));
<INITIAL>&    => (Tokens.AND(yypos, yypos + size yytext));
<INITIAL>\>\= => (Tokens.GE(yypos, yypos + size yytext));
<INITIAL>\>   => (Tokens.GT(yypos, yypos + size yytext));
<INITIAL>\<\= => (Tokens.LE(yypos, yypos + size yytext));
<INITIAL>\<   => (Tokens.LT(yypos, yypos + size yytext));
<INITIAL>\<\> => (Tokens.NEQ(yypos, yypos + size yytext));
<INITIAL>\=   => (Tokens.EQ(yypos, yypos + size yytext));
<INITIAL>\/   => (Tokens.DIVIDE(yypos, yypos + size yytext));
<INITIAL>\*   => (Tokens.TIMES(yypos, yypos + size yytext));
<INITIAL>-    => (Tokens.MINUS(yypos, yypos + size yytext));
<INITIAL>\+   => (Tokens.PLUS(yypos, yypos + size yytext));
<INITIAL>\.   => (Tokens.DOT(yypos, yypos + size yytext));
<INITIAL>\}   => (Tokens.RBRACE(yypos, yypos + size yytext));
<INITIAL>\{   => (Tokens.LBRACE(yypos, yypos + size yytext));
<INITIAL>\]   => (Tokens.RBRACK(yypos, yypos + size yytext));
<INITIAL>"\[" => (Tokens.LBRACK(yypos, yypos + size yytext));
<INITIAL>\)   => (Tokens.RPAREN(yypos, yypos + size yytext));
<INITIAL>\(   => (Tokens.LPAREN(yypos, yypos + size yytext));
<INITIAL>\;   => (Tokens.SEMICOLON(yypos, yypos + size yytext));
<INITIAL>:    => (Tokens.COLON(yypos, yypos + size yytext));
<INITIAL>,    => (Tokens.COMMA(yypos, yypos + size yytext));

<INITIAL>{digit}+  => (Tokens.INT(atoi yytext, yypos, yypos + size yytext));
<INITIAL>{id}     => (Tokens.ID(yytext, yypos, yypos + size yytext));
<INITIAL>[ \t]*   => (continue());

<INITIAL>\"              => (YYBEGIN STRING; SrcString.new(yypos); continue());
<STRING>\"               => (YYBEGIN INITIAL; SrcString.emit(yypos));
<STRING>\\               => (YYBEGIN ESCAPE; continue());
<ESCAPE>n                => (SrcString.pushString("\n", yypos); YYBEGIN STRING; continue());
<ESCAPE>t                => (SrcString.pushString("\t", yypos); YYBEGIN STRING; continue());
<ESCAPE>\\               => (SrcString.pushString("\\", yypos); YYBEGIN STRING; continue());
<ESCAPE>{digit}{3}       => (SrcString.pushAscii(yytext, yypos); YYBEGIN STRING;  continue());
<ESCAPE>\^.              => (SrcString.pushControl(yytext, yypos); YYBEGIN STRING; continue());
<ESCAPE>{whitespace}*\\  => (YYBEGIN STRING; continue());
<STRING>.                => (SrcString.pushString(yytext, yypos); continue());


<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; incComment(); continue());
<COMMENT>"*/"         => (decComment(); if !commentDepth=0 then YYBEGIN INITIAL else (); continue());
<COMMENT>"\n"         => (Newline.add(yypos); continue());
<COMMENT>.            => (continue());

"\n" => (Newline.add(yypos); continue());
.    => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

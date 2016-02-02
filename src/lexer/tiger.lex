type pos = int
type lexresult = Token.token

exception StringFailed;

fun eof() =
    (if !SrcString.buildingString then raise SrcString.StringNotClosed(Newline.getPos(!SrcString.startPos), Newline.getLine(!SrcString.startPos))
    else Token.EOF)

fun atoi(a) =
    valOf (Int.fromString a)

%%

%s ERROR ESCAPE COMMENT STRING;

digit = [0-9];
id = [a-zA-Z][a-zA-Z0-9_]*;
alpha = [a-zA-Z];
whitespace = [\t\r ];

%%
<INITIAL>type     => (Token.TYPE(yypos, yypos + size yytext));
<INITIAL>var      => (Token.VAR(yypos, yypos + size yytext));
<INITIAL>function => (Token.FUNCTION(yypos, yypos + size yytext));
<INITIAL>break    => (Token.BREAK(yypos, yypos + size yytext));
<INITIAL>of       => (Token.OF(yypos, yypos + size yytext));
<INITIAL>end      => (Token.END(yypos, yypos + size yytext));
<INITIAL>in       => (Token.IN(yypos, yypos + size yytext));
<INITIAL>nil      => (Token.NIL(yypos, yypos + size yytext));
<INITIAL>let      => (Token.LET(yypos, yypos + size yytext));
<INITIAL>do       => (Token.DO(yypos, yypos + size yytext));
<INITIAL>to       => (Token.TO(yypos, yypos + size yytext));
<INITIAL>for      => (Token.FOR(yypos, yypos + size yytext));
<INITIAL>while    => (Token.WHILE(yypos, yypos + size yytext));
<INITIAL>else     => (Token.ELSE(yypos, yypos + size yytext));
<INITIAL>then     => (Token.THEN(yypos, yypos + size yytext));
<INITIAL>if       => (Token.IF(yypos, yypos + size yytext));
<INITIAL>array    => (Token.ARRAY(yypos, yypos + size yytext));

<INITIAL>:\=  => (Token.ASSIGN(yypos, yypos + size yytext));
<INITIAL>\|   => (Token.OR(yypos, yypos + size yytext));
<INITIAL>&    => (Token.AND(yypos, yypos + size yytext));
<INITIAL>\>\= => (Token.GE(yypos, yypos + size yytext));
<INITIAL>\>   => (Token.GT(yypos, yypos + size yytext));
<INITIAL>\<\= => (Token.LE(yypos, yypos + size yytext));
<INITIAL>\<   => (Token.LT(yypos, yypos + size yytext));
<INITIAL>\<\> => (Token.NEQ(yypos, yypos + size yytext));
<INITIAL>\=   => (Token.EQ(yypos, yypos + size yytext));
<INITIAL>\/   => (Token.DIVIDE(yypos, yypos + size yytext));
<INITIAL>\*   => (Token.TIMES(yypos, yypos + size yytext));
<INITIAL>-    => (Token.MINUS(yypos, yypos + size yytext));
<INITIAL>\+   => (Token.PLUS(yypos, yypos + size yytext));
<INITIAL>\.   => (Token.DOT(yypos, yypos + size yytext));
<INITIAL>\}   => (Token.RBRACE(yypos, yypos + size yytext));
<INITIAL>\{   => (Token.LBRACE(yypos, yypos + size yytext));
<INITIAL>\]   => (Token.RBRACK(yypos, yypos + size yytext));
<INITIAL>"\[" => (Token.LBRACK(yypos, yypos + size yytext));
<INITIAL>\)   => (Token.RPAREN(yypos, yypos + size yytext));
<INITIAL>\(   => (Token.LPAREN(yypos, yypos + size yytext));
<INITIAL>\;   => (Token.SEMICOLON(yypos, yypos + size yytext));
<INITIAL>:    => (Token.COLON(yypos, yypos + size yytext));
<INITIAL>,    => (Token.COMMA(yypos, yypos + size yytext));

<INITIAL>{digit}+  => (Token.INT(atoi yytext, yypos, yypos + size yytext));
<INITIAL>{id}     => (Token.ID(yytext, yypos, yypos + size yytext));
<INITIAL>[ \t]*   => (continue());

<INITIAL>\"              => (YYBEGIN STRING; SrcString.new(yypos); continue());
<STRING>\"               => (YYBEGIN INITIAL; SrcString.emit(yypos));
<STRING>\\               => (YYBEGIN ESCAPE; continue());
<ESCAPE>n                => (SrcString.pushString("\n", yypos); YYBEGIN STRING; continue());
<ESCAPE>t                => (SrcString.pushString("\t", yypos); YYBEGIN STRING; continue());
<ESCAPE>\\               => (SrcString.pushString("\\", yypos); YYBEGIN STRING; continue());
<ESCAPE>{digit}{3}       => (SrcString.pushAscii(yytext, yypos); YYBEGIN STRING;  continue());
<ESCAPE>\^.              => (SrcString.pushControl(yytext, yypos); YYBEGIN STRING; continue());
<ESCAPE>"\n"             => (Newline.add(yypos); continue());
<ESCAPE>{whitespace}*\\  => (YYBEGIN STRING; continue());
<ESCAPE>.                => (YYBEGIN ERROR; ErrorMsg.error yypos ("illegal character in escape " ^ yytext); continue());
<ERROR>.*\"              => (YYBEGIN INITIAL; continue());
<STRING>.                => (SrcString.pushString(yytext, yypos); continue());

<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; SrcComment.start(yypos); continue());
<COMMENT>"*/"         => (SrcComment.finish(yypos); if SrcComment.closed() then YYBEGIN INITIAL else (); continue());
<COMMENT>.            => (continue());

"\n" => (Newline.add(yypos); continue());
.    => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());

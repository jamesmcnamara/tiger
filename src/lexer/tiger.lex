type pos = int
type lexresult = Token.token

fun eof() =
    (if not (SrcString.closed()) then
        (ErrorMsg.error (SrcString.getStartPos()) ("String not closed");
         raise SrcString.StringNotClosed(SrcString.getStartPos()))
     else if not (SrcComment.closed()) then
        (ErrorMsg.error (SrcComment.getStartPos()) ("Comment not closed");
         raise SrcComment.CommentNotClosed(SrcComment.getStartPos()))
    else Token.EOF)

fun atoi(a) =
    valOf (Int.fromString a)

fun sub1 n =
    n - 1

%%

%s ERROR ESCAPE COMMENT STRING;

digit = [0-9];
id = [a-zA-Z][a-zA-Z0-9_]*;
alpha = [a-zA-Z];
whitespace = [\t\r ];

%%
<INITIAL>:\=  => (Token.ASSIGN(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\|   => (Token.OR(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>&    => (Token.AND(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\>\= => (Token.GE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\>   => (Token.GT(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\<\= => (Token.LE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\<   => (Token.LT(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\<\> => (Token.NEQ(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\=   => (Token.EQ(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\/   => (Token.DIVIDE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\*   => (Token.TIMES(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>-    => (Token.MINUS(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\+   => (Token.PLUS(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\.   => (Token.DOT(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\}   => (Token.RBRACE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\{   => (Token.LBRACE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\]   => (Token.RBRACK(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>"\[" => (Token.LBRACK(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\)   => (Token.RPAREN(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\(   => (Token.LPAREN(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\;   => (Token.SEMICOLON(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>:    => (Token.COLON(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>,    => (Token.COMMA(sub1 yypos, sub1 yypos + size yytext));

<INITIAL>{digit}+ => (Token.int (atoi yytext) (sub1 yypos, sub1 yypos + size yytext));
<INITIAL>{id}     => (Token.find(yytext, sub1 yypos, sub1 yypos + size yytext));
<INITIAL>[ \t]*   => (continue());

<INITIAL>\"              => (YYBEGIN STRING; SrcString.new(sub1 yypos); continue());
<STRING>\"               => (YYBEGIN INITIAL; SrcString.emit(sub1 yypos));
<STRING>\\               => (YYBEGIN ESCAPE; continue());
<ESCAPE>n                => (SrcString.pushString("\n", sub1 yypos); YYBEGIN STRING; continue());
<ESCAPE>t                => (SrcString.pushString("\t", sub1 yypos); YYBEGIN STRING; continue());
<ESCAPE>\\               => (SrcString.pushString("\\", sub1 yypos); YYBEGIN STRING; continue());
<ESCAPE>{digit}{3}       => (SrcString.pushAscii(yytext, sub1 yypos); YYBEGIN STRING;  continue());
<ESCAPE>\^.              => (SrcString.pushControl(yytext, sub1 yypos); YYBEGIN STRING; continue());
<ESCAPE>"\n"             => (Newline.add(sub1 yypos); continue());
<ESCAPE>{whitespace}*\\  => (YYBEGIN STRING; continue());
<ESCAPE>.                => (YYBEGIN ERROR; ErrorMsg.error (sub1 yypos) ("illegal character in escape " ^ yytext); continue());
<ERROR>.*\"              => (YYBEGIN INITIAL; continue());
<STRING>.                => (SrcString.pushString(yytext, sub1 yypos); continue());

<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; SrcComment.start(sub1 yypos); continue());
<COMMENT>"*/"         => (SrcComment.finish(sub1 yypos); if SrcComment.closed() then YYBEGIN INITIAL else (); continue());
<COMMENT>.            => (continue());

"\n" => (Newline.add(sub1 yypos); continue());
.    => (ErrorMsg.error (sub1 yypos) ("illegal character " ^ yytext); continue());

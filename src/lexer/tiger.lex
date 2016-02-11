type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue,pos) token

structure SrcString = SrcStringFun(Tokens)
structure SrcId = SrcIdFun(Tokens)

fun eof() =
    (if not (SrcString.closed()) then raise SrcString.StringNotClosed(SrcString.getStartPos())
    else if not (SrcComment.closed()) then raise SrcComment.CommentNotClosed(SrcComment.getStartPos())
    else Tokens.EOF(1,2))

fun atoi(a) =
    valOf (Int.fromString a)

fun sub1 n =
    n - 1

%%

%header (functor TigerLexFun(structure Tokens : Tiger_TOKENS));

%s ERROR ESCAPE COMMENT STRING;

digit = [0-9];
id = [a-zA-Z][a-zA-Z0-9_]*;
alpha = [a-zA-Z];
whitespace = [\t\r ];

%%
<INITIAL>:\=  => (Tokens.ASSIGN(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\|   => (Tokens.OR(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>&    => (Tokens.AND(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\>\= => (Tokens.GE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\>   => (Tokens.GT(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\<\= => (Tokens.LE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\<   => (Tokens.LT(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\<\> => (Tokens.NEQ(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\=   => (Tokens.EQ(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\/   => (Tokens.DIVIDE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\*   => (Tokens.TIMES(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>-    => (Tokens.MINUS(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\+   => (Tokens.PLUS(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\.   => (Tokens.DOT(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\}   => (Tokens.RBRACE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\{   => (Tokens.LBRACE(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\]   => (Tokens.RBRACK(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>"\[" => (Tokens.LBRACK(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\)   => (Tokens.RPAREN(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\(   => (Tokens.LPAREN(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>\;   => (Tokens.SEMICOLON(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>:    => (Tokens.COLON(sub1 yypos, sub1 yypos + size yytext));
<INITIAL>,    => (Tokens.COMMA(sub1 yypos, sub1 yypos + size yytext));

<INITIAL>{digit}+ => (Tokens.INT(atoi yytext, sub1 yypos, sub1 yypos + size yytext));
<INITIAL>{id}     => (SrcId.lookup(yytext, sub1 yypos, sub1 yypos + size yytext));
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

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

%% 
%s IGNORE COMMENT STRING;

digit = [0-9]+;
id = [a-zA-Z][a-zA-Z0-9_]*;

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

<INITIAL>{digit}  => (Tokens.INT(atoi yytext, yypos, yypos + size yytext));
<INITIAL>{id}     => (Tokens.ID(yytext, yypos, size yytext));
<INITIAL>[ \t]*   => (continue());

<INITIAL>\"  => (YYBEGIN STRING; continue());
<STRING>\"  => (YYBEGIN INITIAL; continue());
<STRING>\\\" => (continue());
<STRING>\\   => (YYBEGIN IGNORE; continue());
<IGNORE>\\   => (YYBEGIN STRING; continue());


<INITIAL,COMMENT>"/*" => (YYBEGIN COMMENT; incComment(); continue());
<COMMENT>"*/"         => (YYBEGIN INITIAL; decComment(); continue());
<COMMENT>.            => (continue());

"\n" => (handle_newline(continue, yypos, yytext));

.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());


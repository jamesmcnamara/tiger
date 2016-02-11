(*structure Lexer :> LEXER =
struct
    fun run lexer =
        let val t = lexer ()
        in
            if Token.isEof(t) then [t] else t::(run lexer)
        end

    fun lexFile filename =
        let val file = TextIO.openIn filename
            fun get _ = TextIO.input file
            val lexer = Mlex.makeLexer get
        in
            Token.reset();
            ErrorMsg.reset();
            Newline.reset();
            SrcComment.reset();
            SrcString.reset();
            run lexer
        end
end*)

structure Lexer =
struct
  structure TigerLrVals = TigerLrValsFun(structure Token = LrParser.Token)
  structure Lex = TigerLexFun(structure Tokens = TigerLrVals.Tokens)
  structure SrcString = SrcStringFun(TigerLrVals.Tokens)
  structure SrcId = SrcIdFun(TigerLrVals.Tokens)

  fun parse filename =
      let val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
	      val file = TextIO.openIn filename
	      fun get _ = TextIO.input file
	      val lexer = Lex.makeLexer get
       in
        ErrorMsg.reset();
        Newline.reset();
        SrcComment.reset();
        SrcId.reset();
        SrcString.reset();
        let val t = lexer ()
        in
            TextIO.closeIn file;
            t
        end
      end
end

structure Lexer :> LEXER =
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
end

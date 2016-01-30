structure Lexer =
struct
    fun run lexer =
        let val t = lexer ()
        in
            case t of
                Tokens.EOF => [Tokens.EOF]
              | t => t::(run lexer)
        end

    fun lexFile filename =
        let val file = TextIO.openIn filename
            fun get _ = TextIO.input file
            val lexer = Mlex.makeLexer get
        in
            ErrorMsg.reset();
            Newline.reset();
            run lexer
        end

    fun lexString string =
        (* TODO: This is not correct at all. *)
        let val lexer = Mlex.makeLexer (fn n => string)
        in
            ErrorMsg.reset();
            Newline.reset();
            run lexer
        end

end

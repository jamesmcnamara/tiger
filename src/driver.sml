structure Parse =
struct

    fun run lexer =
        let val t = lexer ()
        in
            case t of
                Tokens.EOF(i,j,l) => [Tokens.EOF(i,j,l)]
              | t => t::(run lexer)
        end

    fun parseFile filename =
        let val file = TextIO.openIn filename
            fun get _ = TextIO.input file
            val lexer = Mlex.makeLexer get
        in run lexer
        end

    fun parseString string =
        (* TODO: This is not correct at all. *)
        let val lexer = Mlex.makeLexer (fn n => string)
        in run lexer
        end

end

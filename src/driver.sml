structure Parse =
struct

    fun run lexer =
        let val t = lexer ()
        in print t; print "\n";
           if substring (t, 0, 3) = "EOF" then () else (run lexer)
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

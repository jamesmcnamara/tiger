structure Parse =
struct

    fun run lexer =
        let val t = lexer ()
        in print t; print "\n";
           if substring (t, 0, 3) = "EOF" then () else (run lexer)
        end

    fun parse_file filename =
        let val file = TextIO.openIn filename
            fun get _ = TextIO.input file
            val lexer = Mlex.makeLexer get
        in run lexer
        end

    (* TODO: parse_string *)

end

signature LEXER =
sig
    val parseFile : string -> [Token.token]
    val parseString : string -> [Token.token]
end

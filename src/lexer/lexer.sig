signature LEXER =
sig
    val lexFile : string -> Token.token list
    val lexString : string -> Token.token list
end

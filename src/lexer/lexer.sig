signature LEXER =
sig
    val lexFile : string -> Tokens.token list
    val lexString : string -> Tokens.token list
end

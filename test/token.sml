Test.test (fn () =>
    let val tokens = Lexer.lexFile "fixtures/test1.tig"
        val token = Token.LET(43, 46)
    in
        Test.assertEq(token, hd tokens, Token.toString)
    end
);

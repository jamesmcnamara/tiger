Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/one.tig"
        val s = "string"
    in
        Test.assertEq(Tokens.STRING(s,1,8), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/two.tig"
        val s = "string"
    in
        Test.assertEq(Tokens.STRING(s,1,8), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/three.tig"
        val s = "string"
    in
        Test.assertEq(Tokens.STRING(s,1,8), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/one.tig"
        val s = "hello world"
    in
        Test.assertEq(Tokens.STRING(s,1,8), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/two.tig"
        val s = "hello world"
    in
        Test.assertEq(Tokens.STRING(s,1,8), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/three.tig"
        val s = "hello world"
    in
        Test.assertEq(Tokens.STRING(s,1,8), List.hd(tokens), Tokens.toString)
    end
);

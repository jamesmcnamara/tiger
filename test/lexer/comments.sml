Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/one.tig"
    in
        Test.assertEq(2, List.length(tokens), Int.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/two.tig"
    in
        Test.assertEq(2, List.length(tokens), Int.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/three.tig"
    in
        Test.assertEq(2, List.length(tokens), Int.toString)
    end
);

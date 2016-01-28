Test.test (fn () =>
    let val tokens = Parse.parseFile "fixtures/test1.tig"
        val expected = [
            Tokens.LET(1,3,2),
            Tokens.TYPE(2,4,3),
            Tokens.ID("arrtype",8,7,3),
            Tokens.EQ(16,1,3),
            Tokens.ARRAY(18,5,3),
            Tokens.OF(24,2,3),
            Tokens.ID("int",27,3,3),
            Tokens.VAR(2,3,4),
            Tokens.ID("arr1",6,4,4),
            Tokens.COLON(10,1,4),
            Tokens.ID("arrtype",11,7,4),
            Tokens.ASSIGN(19,2,4),
            Tokens.ID("arrtype",22,7,4),
            Tokens.LBRACK(30,1,4),
            Tokens.INT(10,31,2,4),
            Tokens.RBRACK(33,1,4),
            Tokens.OF(35,2,4),
            Tokens.INT(0,38,1,4),
            Tokens.IN(1,2,5),
            Tokens.ID("arr1", 2,4,6),
            Tokens.END(1,3,7),
            Tokens.EOF(0,0,8)
        ]
    in
        Test.assertEq(expected,tokens)
    end
);

Test.test (fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/comments/three.tig"
        val expected = [
            Tokens.STRING("derp",1,4,4),
            Tokens.EOF(0,0,5)
        ]
    in
        Test.assertEq(expected,tokens)
    end
);

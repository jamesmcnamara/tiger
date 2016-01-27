Test.test (fn () =>
    let val tokens = Parse.parseFile "fixtures/test1.tig"
    in
        let val expected = [Tokens.TYPE(2,4,3),Tokens.EQ(16,1,3),Tokens.OF(24,2,3),Tokens.VAR(2,3,4),Tokens.COLON(10,1,4),Tokens.ASSIGN(19,2,4),Tokens.LBRACK(30,1,4),Tokens.RBRACK(33,1,4),Tokens.INT(0,38,1,4),Tokens.ID("arr1", 2,4,6),Tokens.EOF(0,0,8)]
        in
             Test.assert(tokens = expected))
        end
    end
);

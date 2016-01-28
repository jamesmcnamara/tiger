Test.test (fn () =>
    let val tokens = Parse.parseFile "fixtures/test1.tig"
    in
        Test.assertEq([],tokens)
    end
);

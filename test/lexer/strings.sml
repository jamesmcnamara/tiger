Test.test (fn () =>
    let val tokens = Parse.parseFile "fixtures/test1.tig"
    in
        (print (String.concatWith ", " (map Tokens.toString tokens));
         Test.assert(tokens = []))
    end
);

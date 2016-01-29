Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/strings/one.tig"
        val string = "hello world"
    in
        Test.assertEq(Tokens.STRING(string,1,11), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/strings/two.tig"
        val string = "hello newline\n"
    in
        Test.assertEq(Tokens.STRING(string,1,15), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/strings/three.tig"
        val string = "this one has \120 ascii"
    in
        Test.assertEq(Tokens.STRING(string,1,23), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/strings/four.tig"
        val string = "hello plus @#^! weird chars!"
    in
        Test.assertEq(Tokens.STRING(string,1,28), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/strings/five.tig"
        val string = "this has \\ and \n newline and !@$^ curse words!"
    in
        Test.assertEq(Tokens.STRING(string,1,48), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/strings/six.tig"
        val string = "this has control sequence \^C for end of text"
    in
        Test.assertEq(Tokens.STRING(string,1,45), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Parse.parseFile "fixtures/lexer/strings/seven.tig"
        val string = "this is a multiline\    \ string!"
    in
        Test.assertEq(Tokens.STRING(string,1,33), List.hd(tokens), Tokens.toString)
    end
);

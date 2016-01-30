Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/strings/one.tig"
        val string = "hello world"
    in
        Test.assertEq(Tokens.STRING(string,1,13), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/strings/two.tig"
        val string = "hello newline\n"
    in
        Test.assertEq(Tokens.STRING(string,1,17), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/strings/three.tig"
        val string = "this one has \120 ascii"
    in
        Test.assertEq(Tokens.STRING(string,1,25), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/strings/four.tig"
        val string = "hello plus @#^! weird chars!"
    in
        Test.assertEq(Tokens.STRING(string,1,30), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/strings/five.tig"
        val string = "this has \\ and \n newline and !@$^ curse words!"
    in
        Test.assertEq(Tokens.STRING(string,1,50), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/strings/six.tig"
        val string = "this has control sequence \^C for end of text"
    in
        Test.assertEq(Tokens.STRING(string,1,47), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/strings/seven.tig"
        val string = "this is a multiline\    \ string!"
    in
        Test.assertEq(Tokens.STRING(string,1,35), List.hd(tokens), Tokens.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/comments/one.tig"
        val s = "hello world"
    in
        Test.assertEq(Token.STRING(s, 25, 37), List.hd(tokens), Token.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/comments/two.tig"
        val s = "hello world"
    in
        Test.assertEq(Token.STRING(s, 43, 55), List.hd(tokens), Token.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/comments/three.tig"
        val s = "hello world"
    in
        Test.assertEq(Token.STRING(s, 36, 48), List.hd(tokens), Token.toString);
        Test.assertEq(4, Newline.getLine(36), Int.toString);
        Test.assertEq(1, Newline.getPos(36), Int.toString)
    end
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/comments/four.tig"
    in
        Test.assert(false)
    end
    handle SrcComment.CommentNotClosed(n) => Test.assertEq(1, n, Int.toString)
        | _ => Test.assert(false)
);

Test.test(fn () =>
    let val tokens = Lexer.lexFile "fixtures/lexer/comments/five.tig"
    in
        Test.assert(false)
    end
    handle SrcComment.CommentNotClosed(n) => Test.assertEq(15, n, Int.toString)
        | _ => Test.assert(false)
);

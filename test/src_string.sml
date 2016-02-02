Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushString("hello", 1);
     Test.assertEq(Token.string "hello" (1, 10),
                   SrcString.emit(10),
                   Token.toString))
);

Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushAscii("067", 1);
     Test.assertEq(Token.string "C" (1, 10),
                   SrcString.emit(10),
                   Token.toString))
);

Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushString("hi ", 1);
     SrcString.pushControl("^C", 4);
     Test.assertEq(Token.string "hi \^C" (1, 10),
                   SrcString.emit(10),
                   Token.toString))
);

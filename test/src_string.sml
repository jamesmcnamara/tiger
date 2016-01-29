Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushString("hello", 1);
     Test.assertEq(Tokens.STRING("hello",1,10),
                   SrcString.emit(10),
                   Tokens.toString))
);

Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushAscii("067", 1);
     Test.assertEq(Tokens.STRING("C",1,10),
                   SrcString.emit(10),
                   Tokens.toString))
);

Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushString("hi ", 1);
     SrcString.pushControl("^C", 4);
     Test.assertEq(Tokens.STRING("hi \^C",1,10),
                   SrcString.emit(10),
                   Tokens.toString))
);

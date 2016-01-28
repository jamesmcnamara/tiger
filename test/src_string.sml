Test.test (fn () =>
    (SrcString.new 1;
     SrcString.push("s", 10);
     Test.assertEq(Tokens.STRING("s",1,13,1),
                   SrcString.emit(),
                   Tokens.toString))
);

(*Test.test (fn () =>
    (SrcString.new 1;
     SrcString.pushString("hello world");
     Test.assertEq(Tokens.STRING("s",1,11,1),
                   SrcString.emit(),
                   Tokens.toString))
);*)

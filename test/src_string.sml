Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.push("s", 10);
     Test.assertEq(Tokens.STRING("s",1,10,1),
                   SrcString.emit(10),
                   Tokens.toString))
);

Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushString("hello");
     Test.assertEq(Tokens.STRING("hello",1,10,1),
                   SrcString.emit(10),
                   Tokens.toString))
);

Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushAscii("067");
     (*
     "\067"
     123456
     *)
     Test.assertEq(Tokens.STRING("C",1,10,1),
                   SrcString.emit(10),
                   Tokens.toString))
);

Test.test (fn () =>
    (Newline.reset();
     SrcString.new 1;
     SrcString.pushString("hi ");
     SrcString.pushControl("^C");
     (*
     "hi \^C"
     12345678
     *)
     Test.assertEq(Tokens.STRING("hi \^C",1,10,1),
                   SrcString.emit(10),
                   Tokens.toString))
);

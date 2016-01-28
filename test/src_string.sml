Test.test (fn () =>
    (Tracking.reset();
     SrcString.new 1;
     SrcString.push("s", 10);
     Test.assertEq(Tokens.STRING("s",1,12,1),
                   SrcString.emit(),
                   Tokens.toString))
);

Test.test (fn () =>
    (Tracking.reset();
     SrcString.new 1;
     SrcString.pushString("hello");
     (*
     "hello"
     1234567
     *)
     Test.assertEq(Tokens.STRING("hello",1,7,1),
                   SrcString.emit(),
                   Tokens.toString))
);

Test.test (fn () =>
    (Tracking.reset();
     SrcString.new 1;
     SrcString.pushAscii("067");
     (*
     "\067"
     123456
     *)
     Test.assertEq(Tokens.STRING("C",1,6,1),
                   SrcString.emit(),
                   Tokens.toString))
);

Test.test (fn () =>
    (Tracking.reset();
     SrcString.new 1;
     SrcString.pushString("hi ");
     SrcString.pushControl("^C");
     (*
     "hi \^C"
     12345678
     *)
     Test.assertEq(Tokens.STRING("hi \^C",1,8,1),
                   SrcString.emit(),
                   Tokens.toString))
);

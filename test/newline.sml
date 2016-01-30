Test.test(fn () =>
    (Newline.reset();
     Newline.add(5);
     Test.assertEq(1, Newline.getLine(1), Int.toString);
     Test.assertEq(1, Newline.getLine(4), Int.toString);
     Test.assertEq(2, Newline.getLine(5), Int.toString);
     Test.assertEq(2, Newline.getLine(6), Int.toString);
     Newline.add(20);
     Test.assertEq(2, Newline.getLine(19), Int.toString);
     Test.assertEq(3, Newline.getLine(20), Int.toString);
     Test.assertEq(3, Newline.getLine(21), Int.toString))
);

Test.test(fn () =>
    (Newline.reset();
     Newline.add(5);
     Test.assertEq(0, Newline.getPos(1), Int.toString);
     Test.assertEq(3, Newline.getPos(4), Int.toString);
     Test.assertEq(0, Newline.getPos(5), Int.toString);
     Test.assertEq(1, Newline.getPos(6), Int.toString);
     Newline.add(20);
     Test.assertEq(14, Newline.getPos(19), Int.toString);
     Test.assertEq(0, Newline.getPos(20), Int.toString);
     Test.assertEq(1, Newline.getPos(21), Int.toString))
);

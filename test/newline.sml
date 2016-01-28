Test.test(fn () =>
    (Newline.reset();
     Newline.add(5);
     Newline.add(23);
     Test.assert(1 = Newline.getLine(1));
     Test.assert(1 = Newline.getLine(5));
     Test.assert(2 = Newline.getLine(6));
     Test.assert(2 = Newline.getLine(22));
     Test.assert(3 = Newline.getLine(28)))
);

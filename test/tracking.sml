Test.test(fn () =>
    (Tracking.reset();
     Tracking.newline(5);
     Tracking.newline(23);
     Test.assert(1 = Tracking.getLine(1));
     Test.assert(1 = Tracking.getLine(5));
     Test.assert(2 = Tracking.getLine(6));
     Test.assert(2 = Tracking.getLine(22));
     Test.assert(3 = Tracking.getLine(28)))
);

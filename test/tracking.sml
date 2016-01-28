Test.test(fn () =>
    (Tracking.reset();
    Tracking.shift(2);
    Test.assert(3 = Tracking.getPosition());
    Test.assert(1 = Tracking.getLine());
    Tracking.newline();
    Test.assert(1 = Tracking.getPosition());
    Test.assert(2 = Tracking.getLine());
    true)
);

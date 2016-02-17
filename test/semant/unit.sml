Test.test(
    "semant simple one",
    fn () =>
       let val actual = Main.compile "fixtures/semant/one.tig"
           val expected = {exp=(), ty=Types.INT}
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "semant simple two",
    fn () =>
       let val actual = Main.compile "fixtures/semant/two.tig"
           val expected = {exp=(), ty=Types.STRING}
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "semant simple three",
    fn () =>
       let val actual = Main.compile "fixtures/semant/three.tig"
           val expected = {exp=(), ty=Types.NIL}
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "semant simple four",
    fn () =>
       let val actual = Main.compile "fixtures/semant/four.tig"
           val expected = {exp=(), ty=Types.INT}
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "semant simple five",
    fn () =>
       (Main.compile "fixtures/semant/five.tig"; Test.assert(false))
       handle Semant.TypeError(_,_,_) => Test.assert(true));

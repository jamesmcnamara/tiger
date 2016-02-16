Test.test(fn () =>
    let val actual = Main.compile "fixtures/parser/simple/one.tig"
        val expected = {exp=(), ty=Types.INT}
    in
        Test.assert(expected = actual)
    end
);

Test.test(fn () =>
    let val actual = Main.compile "fixtures/parser/simple/two.tig"
        val expected = {exp=(), ty=Types.STRING}
    in
        Test.assert(expected = actual)
    end
);

Test.test(fn () =>
    let val actual = Main.compile "fixtures/parser/simple/three.tig"
        val expected = {exp=(), ty=Types.NIL}
    in
        Test.assert(expected = actual)
    end
);

let open Absyn
    val astPrint = (fn ast => PrintAbsyn.print(TextIO.stdOut, ast)) in

Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/harder/one.tig"
        val expected = OpExp { pos=1,
                               left=IntExp(1),
                               oper=PlusOp,
                               right=OpExp { pos=5,
                                             left=IntExp(1),
                                             oper=TimesOp,
                                             right=IntExp(2) }}
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
);

Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/harder/two.tig"
        val expected = AssignExp { pos=1,
                                   var=SimpleVar(Symbol.symbol("a"),1),
                                   exp=ArrayExp { pos=6,
                                                  typ=Symbol.symbol("b"),
                                                  size=IntExp(2),
                                                  init=IntExp(1) }}
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
);

Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/harder/three.tig"
        val expected = VarExp(SubscriptVar(SimpleVar(Symbol.symbol("a"),1),
                                           IntExp(3),
                                           1))
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
);

(*a[3] := b[1] of 2*)
Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/harder/four.tig"
        val expected = AssignExp { pos=1,
                                   var=SubscriptVar(SimpleVar(Symbol.symbol("a"),1),
                                                    IntExp(3),
                                                    1),
                                   exp=ArrayExp { pos=9,
                                                  typ=Symbol.symbol("b"),
                                                  size=IntExp(1),
                                                  init=IntExp(2) }}
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
);

true
end

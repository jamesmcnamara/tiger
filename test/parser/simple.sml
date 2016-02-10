let val astPrint = (fn ast => PrintAbsyn.print(TextIO.stdOut, ast))
in
(Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/one.tig"
        val expected = Absyn.IntExp(1)
    in
         Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
     let val actual = Parse.parse "fixtures/parser/simple/two.tig"
         val expected = Absyn.StringExp("hello",1)
     in
          Test.assertEqIO(expected, actual, astPrint)
     end
  )
);
Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/three.tig"
        val expected = Absyn.NilExp
    in
         Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
     let val actual = Parse.parse "fixtures/parser/simple/four.tig"
         val expected = Absyn.NegExp(Absyn.IntExp(123), 1)
     in
          Test.assertEqIO(expected, actual, astPrint)
     end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/five.tig"
        val expected = Absyn.SeqExp([(Absyn.IntExp(123),2),(Absyn.StringExp("hello",6),6)])
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/six.tig"
        val expected = Absyn.SeqExp([
            (Absyn.OpExp {
                left=Absyn.IntExp(1),
                oper=Absyn.PlusOp,
                right=Absyn.IntExp(1),
                pos=2
            },2),
            (Absyn.OpExp {
                left=Absyn.IntExp(1),
                oper=Absyn.AndOp,
                right=Absyn.IntExp(1),
                pos=8
            },8),
            (Absyn.OpExp {
                left=Absyn.IntExp(1),
                oper=Absyn.OrOp,
                right=Absyn.IntExp(1),
                pos=14
            },14)
        ])
    in
            Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/seven.tig"
        val expected = Absyn.CallExp {
            func=Symbol.symbol("foo"),
            args=[],
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/eight.tig"
        val expected = Absyn.CallExp {
            func=Symbol.symbol("foo"),
            args=[Absyn.IntExp(1)],
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/nine.tig"
        val expected = Absyn.ArrayExp {
            typ=Symbol.symbol("intArray"),
            size=Absyn.IntExp(10),
            init=Absyn.IntExp(0),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/ten.tig"
        val expected = Absyn.AssignExp {
            var=Absyn.SubscriptVar(Absyn.SimpleVar(Symbol.symbol("myArray"),1),Absyn.IntExp(10),1),
            exp=Absyn.IntExp(1),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/eleven.tig"
        val expected = Absyn.AssignExp {
            var=Absyn.FieldVar(Absyn.SimpleVar(Symbol.symbol("myRecord"),1),Symbol.symbol("someField"),1),
            exp=Absyn.StringExp("hello",23),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/danglingelse.tig"
        val expected = Absyn.IfExp {
            test=Absyn.IntExp(1),
            then'=Absyn.IfExp {
                test=Absyn.IntExp(2),
                then'=Absyn.IntExp(3),
                else'=Option.SOME(Absyn.IntExp(4)),
                pos=11
            },
            else'=Option.NONE,
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/twelve.tig"
        val expected = Absyn.WhileExp {
            test=Absyn.VarExp(Absyn.SimpleVar(Symbol.symbol("i"),7)),
            body=Absyn.IntExp(1),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/thirteen.tig"
        val expected = Absyn.ForExp {
            var=Symbol.symbol("i"),
            escape=ref false,
            lo=Absyn.IntExp(0),
            hi=Absyn.IntExp(10),
            body=Absyn.CallExp {
                func=Symbol.symbol("foo"),
                args=[],
                pos=21
            },
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 )

end

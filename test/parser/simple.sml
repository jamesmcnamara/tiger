open Absyn;

let val astPrint = (fn ast => PrintAbsyn.print(TextIO.stdOut, ast))
in
(Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/one.tig"
        val expected = IntExp(1)
    in
         Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
     let val actual = Parse.parse "fixtures/parser/simple/two.tig"
         val expected = StringExp("hello",1)
     in
          Test.assertEqIO(expected, actual, astPrint)
     end
  )
);
Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/three.tig"
        val expected = NilExp
    in
         Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
     let val actual = Parse.parse "fixtures/parser/simple/four.tig"
         val expected = OpExp {left=IntExp(0), oper=MinusOp, right=IntExp(123), pos=1}
     in
          Test.assertEqIO(expected, actual, astPrint)
     end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/five.tig"
        val expected = SeqExp([(IntExp(123),2),(StringExp("hello",6),6)])
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/six.tig"
        val expected = SeqExp([
            (OpExp {
                left=IntExp(1),
                oper=PlusOp,
                right=IntExp(1),
                pos=2
            },2),
            (IfExp {
                test=IntExp(1),
                then'=IntExp(1),
                else'=Option.SOME(IntExp(0)),
                pos=8
            },8),
            (IfExp {
                test=IntExp(1),
                then'=IntExp(1),
                else'=Option.SOME(IntExp(1)),
                pos=14
            },14)
        ])
    in
            Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/seven.tig"
        val expected = CallExp {
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
        val expected = CallExp {
            func=Symbol.symbol("foo"),
            args=[IntExp(1)],
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/nine.tig"
        val expected = ArrayExp {
            typ=Symbol.symbol("intArray"),
            size=IntExp(10),
            init=IntExp(0),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/ten.tig"
        val expected = AssignExp {
            var=SubscriptVar(SimpleVar(Symbol.symbol("myArray"),1),IntExp(10),1),
            exp=IntExp(1),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/eleven.tig"
        val expected = AssignExp {
            var=FieldVar(SimpleVar(Symbol.symbol("myRecord"),1),Symbol.symbol("someField"),1),
            exp=StringExp("hello",23),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/danglingelse.tig"
        val expected = IfExp {
            test=IntExp(1),
            then'=IfExp {
                test=IntExp(2),
                then'=IntExp(3),
                else'=Option.SOME(IntExp(4)),
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
        val expected = WhileExp {
            test=VarExp(SimpleVar(Symbol.symbol("i"),7)),
            body=IntExp(1),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/thirteen.tig"
        val expected = ForExp {
            var=Symbol.symbol("i"),
            escape=ref false,
            lo=IntExp(0),
            hi=IntExp(10),
            body=CallExp {
                func=Symbol.symbol("foo"),
                args=[],
                pos=21
            },
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/fourteen.tig"
        val expected = LetExp {
            decs=[
                TypeDec [{
                    name=Symbol.symbol("a"),
                    ty=NameTy(Symbol.symbol("int"),14),
                    pos=5
                }],
                TypeDec [{
                    name=Symbol.symbol("arrayType"),
                    ty=ArrayTy(Symbol.symbol("int"),19),
                    pos=5
                }],
                TypeDec [{
                    name=Symbol.symbol("recordType"),
                    ty=RecordTy [{
                        name=Symbol.symbol("field"),
                        escape=ref false,
                        typ=Symbol.symbol("int"),
                        pos=24
                    }],
                    pos=5
                }],
                VarDec {
                    name=Symbol.symbol("i"),
                    escape=ref false,
                    typ=Option.NONE,
                    init=IntExp(0),
                    pos=5
                },
                FunctionDec [{
                    name=Symbol.symbol("plus5"),
                    params=[{
                        name=Symbol.symbol("num"),
                        escape=ref false,
                        typ=Symbol.symbol("int"),
                        pos=20
                    }],
                    result=Option.SOME(Symbol.symbol("int"),30),
                    body=OpExp {
                        left=VarExp(SimpleVar(Symbol.symbol("num"),9)),
                        oper=PlusOp,
                        right=IntExp(5),
                        pos=13
                    },
                    pos=5
                }]
            ],
            body=IntExp(1),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 );
 Test.test(fn () =>
    let val actual = Parse.parse "fixtures/parser/simple/fifteen.tig"
        val expected = RecordExp {
            fields=[
                (Symbol.symbol("name"),StringExp("derp",17),12),
                (Symbol.symbol("age"),IntExp(1),24)
            ],
            typ=Symbol.symbol("testRecord"),
            pos=1
        }
    in
        Test.assertEqIO(expected, actual, astPrint)
    end
 )


end

open Absyn;

Test.test(
    "syntax simple one",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/one.tig"
           val expected = IntExp(1)
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple two",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/two.tig"
           val expected = StringExp("hello",1)
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple three",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/three.tig"
           val expected = NilExp
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple four",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/four.tig"
           val expected = OpExp { left=IntExp(0),
                                  oper=MinusOp,
                                  right=IntExp(123),
                                  pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple five",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/five.tig"
           val expected = SeqExp([(IntExp(123), 2), (StringExp("hello", 6), 6)])
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple six",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/six.tig"
           val expected = SeqExp([(OpExp { left=IntExp(1),
                                           oper=PlusOp,
                                           right=IntExp(1),
                                           pos=2 }, 2),
                                  (IfExp { test=IntExp(1),
                                           then'=IntExp(1),
                                           else'=Option.SOME(IntExp(0)),
                                           pos=8 }, 8),
                                  (IfExp { test=IntExp(1),
                                           then'=IntExp(1),
                                           else'=Option.SOME(IntExp(1)),
                                           pos=14 }, 14)])
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple seven",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/seven.tig"
           val expected = CallExp { func=Symbol.symbol("foo"),
                                    args=[],
                                    pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple eight",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/eight.tig"
           val expected = CallExp { func=Symbol.symbol("foo"),
                                    args=[IntExp(1)],
                                    pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple nine",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/nine.tig"
           val expected = ArrayExp { typ=Symbol.symbol("intArray"),
                                     size=IntExp(10),
                                     init=IntExp(0),
                                     pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple ten",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/ten.tig"
           val simpleVar = SimpleVar(Symbol.symbol("myArray"), 1)
           val subscriptVar = SubscriptVar(simpleVar, IntExp(10), 1)
           val expected = AssignExp { var=subscriptVar,
                                      exp=IntExp(1),
                                      pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple eleven",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/eleven.tig"
           val simpleVar = SimpleVar(Symbol.symbol("myRecord"), 1)
           val fieldVar = FieldVar(simpleVar,Symbol.symbol("someField"), 10)
           val expected = AssignExp { var=fieldVar,
                                      exp=StringExp("hello",23),
                                      pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple danglingelse",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/danglingelse.tig"
           val expected = IfExp { test=IntExp(1),
                                  then'=IfExp { test=IntExp(2),
                                                then'=IntExp(3),
                                                else'=Option.SOME(IntExp(4)),
                                                pos=11 },
                                  else'=Option.NONE,
                                  pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple twelve",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/twelve.tig"
           val varExp = VarExp(SimpleVar(Symbol.symbol("i"), 7))
           val expected = WhileExp { test=varExp,
                                     body=IntExp(1),
                                     pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple thirteen",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/thirteen.tig"
           val expected = ForExp { var=Symbol.symbol("i"),
                                   escape=ref false,
                                   lo=IntExp(0),
                                   hi=IntExp(10),
                                   body=CallExp { func=Symbol.symbol("foo"),
                                                  args=[],
                                                  pos=21 },
                                   pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple fourteen",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/fourteen.tig"
           val expected =
               LetExp { decs=[
                          TypeDec [{ name=Symbol.symbol("a"),
                                     ty=NameTy(Symbol.symbol("int"),14),
                                     pos=5 }],
                          TypeDec [{ name=Symbol.symbol("arrayType"),
                                     ty=ArrayTy(Symbol.symbol("int"),19),
                                     pos=5 }],
                          TypeDec [{ name=Symbol.symbol("recordType"),
                                     ty=RecordTy
                                            [{ name=Symbol.symbol("field"),
                                               escape=ref false,
                                               typ=Symbol.symbol("int"),
                                               pos=24 }],
                                     pos=5 }],
                          VarDec { name=Symbol.symbol("i"),
                                   escape=ref false,
                                   typ=Option.NONE,
                                   init=IntExp(0),
                                   pos=5 },
                          FunctionDec [{ name=Symbol.symbol("plus5"),
                                         params=[{ name=Symbol.symbol("num"),
                                                   escape=ref false,
                                                   typ=Symbol.symbol("int"),
                                                   pos=20 }],
                                         result=Option.SOME(Symbol.symbol("int"),30),
                                         body=OpExp { left=VarExp(SimpleVar(Symbol.symbol("num"),9)),
                                                      oper=PlusOp,
                                                      right=IntExp(5),
                                                      pos=13 },
                                         pos=5 }]],
                        body=IntExp(1),
                        pos=1 }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax simple fifteen",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/simple/fifteen.tig"
           val expected =
               RecordExp { fields=[(Symbol.symbol("name"), StringExp("derp",17),12),
                                   (Symbol.symbol("age"),IntExp(1),24)],
                           typ=Symbol.symbol("testRecord"),
                           pos=1 }
       in
           Test.assert(expected = actual)
       end);

open Absyn;

Test.test(
    "syntax harder one",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/harder/one.tig"
           val expected = OpExp { pos=1,
                                  left=IntExp(1),
                                  oper=PlusOp,
                                  right=OpExp { pos=5,
                                                left=IntExp(1),
                                                oper=TimesOp,
                                                right=IntExp(2) }}
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax harder two",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/harder/two.tig"
           val expected = AssignExp { pos=1,
                                      var=SimpleVar(Symbol.symbol("a"),1),
                                      exp=ArrayExp { pos=6,
                                                     typ=Symbol.symbol("b"),
                                                     size=IntExp(2),
                                                     init=IntExp(1) }}
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax harder three",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/harder/three.tig"
           val expected = VarExp(SubscriptVar(SimpleVar(Symbol.symbol("a"),1),
                                              IntExp(3),
                                              2))
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "syntax harder four",
    fn () =>
       let val actual = Parse.parse "fixtures/syntax/harder/four.tig"
           val expected = AssignExp { pos=1,
                                      var=SubscriptVar(SimpleVar(Symbol.symbol("a"),1),
                                                       IntExp(3),
                                                       1),
                                      exp=ArrayExp { pos=9,
                                                     typ=Symbol.symbol("b"),
                                                     size=IntExp(1),
                                                     init=IntExp(2) }}
       in
           Test.assert(expected = actual)
       end);

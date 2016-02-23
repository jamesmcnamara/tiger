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

Test.test(
    "semant simple six",
    fn () =>
       let val actual = Main.compile "fixtures/semant/six.tig"
           val expected = { exp=(), ty=Types.STRING }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "semant simple - call good",
    fn () =>
       let val actual = Main.compile "fixtures/semant/call_good.tig"
           val expected = { exp=(), ty=Types.STRING }
       in
           Test.assert(expected = actual)
       end);

Test.test(
    "semant simple - call bad",
    fn () =>
       (Main.compile "fixtures/semant/call_bad.tig"; Test.assert(false))
       handle Semant.TypeError(_,_,_) => Test.assert(true));

Test.test(
   "semant simple - record bad",
   fn () =>
      (Main.compile "fixtures/semant/record_bad.tig"; Test.assert(false))
      handle Semant.TypeDoesNotExist(_) => Test.assert(true));

Test.test(
 "semant simple - name dec bad",
 fn () =>
    (Main.compile "fixtures/semant/name_bad.tig"; Test.assert(false))
    handle Semant.TypeDoesNotExist(_) => Test.assert(true));

Test.test(
    "semant simple - name dec good",
    fn () =>
      let val actual = Main.compile "fixtures/semant/name_good.tig"
          val expected = { exp=(), ty=Types.UNIT }
      in
          Test.assert(expected = actual)
      end);

Test.test(
    "semant simple - reord dec good",
    fn () =>
    let val actual = Main.compile "fixtures/semant/record_dec_good.tig"
        val expected = { exp=(), ty=Types.UNIT }
    in
        Test.assert(expected = actual)
    end);

Test.test(
    "semant simple - name types",
    fn () =>
    let val actual = Main.compile "fixtures/semant/name_types.tig"
        val expected = { exp=(), ty=Types.UNIT }
    in
        Test.assert(expected = actual)
    end);

Test.test(
    "semant simple - for loop",
    fn () =>
    let val actual = Main.compile "fixtures/semant/for_loop.tig"
        val expected = { exp=(), ty=Types.UNIT }
    in
        Test.assert(expected = actual)
    end);

Test.test(
    "semant simple - while loop",
    fn () =>
    let val actual = Main.compile "fixtures/semant/while_loop.tig"
        val expected = { exp=(), ty=Types.UNIT }
    in
        Test.assert(expected = actual)
    end);

Test.test(
   "semant simple - call bad args",
   fn () =>
      (Main.compile "fixtures/semant/call_bad_args.tig"; Test.assert(false))
      handle Semant.ArityError(_) => Test.assert(true));

Test.test(
   "semant simple - bad record length",
   fn () =>
      (Main.compile "fixtures/semant/record_bad_length.tig"; Test.assert(false))
      handle Semant.ArityError(_) => Test.assert(true));

Test.test(
   "semant simple - bad record field name",
   fn () =>
      (Main.compile "fixtures/semant/bad_record_field_name.tig"; Test.assert(false))
      handle Semant.RecordFieldNameError(_) => Test.assert(true));

Test.test(
    "semant simple - if then",
    fn () =>
    let val actual = Main.compile "fixtures/semant/if_then.tig"
        val expected = { exp=(), ty=Types.UNIT }
    in
        Test.assert(expected = actual)
    end);

Test.test(
   "semant simple - simple var type",
   fn () =>
   let val actual = Main.compile "fixtures/semant/simple_var_type.tig"
      val expected = { exp=(), ty=Types.INT }
   in
      Test.assert(expected = actual)
   end);

Test.test(
   "semant simple - simpler var type",
   fn () =>
   let val actual = Main.compile "fixtures/semant/simpler_var_type.tig"
      val expected = { exp=(), ty=Types.INT }
   in
      Test.assert(expected = actual)
   end);

Test.test(
   "semant simple - bad var type",
   fn () =>
      (Main.compile "fixtures/semant/bad_var_type.tig"; Test.assert(false))
      handle Semant.TypeError(_) => Test.assert(true));

Test.test(
   "semant simple - function",
   fn () =>
   let val actual = Main.compile "fixtures/semant/good_function.tig"
      val expected = { exp=(), ty=Types.INT }
   in
      Test.assert(expected = actual)
   end);

Test.test(
   "semant simple - mutual function",
   fn () =>
   let val actual = Main.compile "fixtures/semant/mutual_function.tig"
      val expected = { exp=(), ty=Types.INT }
   in
      Test.assert(expected = actual)
   end);


Test.test(
   "semant simple - bad mutual function",
   fn () =>
      (Main.compile "fixtures/semant/bad_mutual_function.tig"; Test.assert(false))
      handle Semant.TypeError(_) => Test.assert(true));

Test.test(
   "semant simple - mutual types",
   fn () =>
   let val actual = Main.compile "fixtures/semant/mutual_types.tig"
      val expected = { exp=(), ty=Types.INT }
   in
      Test.assert(expected = actual)
   end);

Test.test(
   "semant simple - field var type",
   fn () =>
   let val actual = Main.compile "fixtures/semant/field_var_type.tig"
      val expected = { exp=(), ty=Types.INT }
   in
      Test.assert(expected = actual)
   end);

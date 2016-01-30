structure Test : TEST =
struct

  val testsRan = ref 0
  val testsPassed = ref 0

  exception Failed

  fun test f = f () handle Failed => false

  fun assertEq(expected,actual,toString) =
    (testsRan := !testsRan + 1;
     if expected = actual
     then (testsPassed := !testsPassed + 1; true)
     else
       let val msg = "Expected: " ^ (toString expected) ^
                     " but got: " ^ (toString actual) ^ ".\n"
       in
         print(msg);
         raise Failed
       end)

  fun assert test =
    (testsRan := !testsRan + 1;
     if test
     then (testsPassed := !testsPassed + 1; true)
     else raise Failed)

  fun reset () =
    (testsRan := 0;
     testsPassed := 0)

  fun printStats () =
    print("\n#### TEST STATISTICS ####\nPASSED: " ^ Int.toString(!testsPassed) ^ "\nFAILED: " ^ Int.toString(!testsRan - !testsPassed) ^ "\n#### END TEST STATISTICS ####\n\n")
end

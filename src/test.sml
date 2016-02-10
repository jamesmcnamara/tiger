structure Test : TEST =
struct
  val testsRan = ref 0
  val testsPassed = ref 0

  exception Failed

  fun test f =
    (testsRan := !testsRan + 1;
     (if f () then (testsPassed := !testsPassed + 1; true) else false)
     handle Failed => false)

  fun assertEq(expected,actual,toString) =
     if expected = actual
     then true
     else
       let val msg = "Expected: " ^ (toString expected) ^
                     " but got: " ^ (toString actual) ^ ".\n"
       in
         print(msg);
         raise Failed
       end

  fun assertEqIO(expected,actual,printer) =
    if expected = actual
    then true
    else
      (print "\nExpected: ";
       printer(expected);
       print "but got: ";
       printer(actual);
       print "\n";
       raise Failed)

  fun assert test =
     if test
     then true
     else raise Failed

  fun reset () =
    (testsRan := 0;
     testsPassed := 0)

  fun printStats () =
    print("\n#### TEST STATISTICS ####\nPASSED: " ^ Int.toString(!testsPassed) ^ "\nFAILED: " ^ Int.toString(!testsRan - !testsPassed) ^ "\n#### END TEST STATISTICS ####\n\n")
end

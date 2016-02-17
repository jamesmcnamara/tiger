signature TEST =
sig
    (* Call this function to test something. A test fails if it throws
     * an un-handled exception. *)
    val test : string * (unit -> unit) -> bool

    (* Report the number of failing and total tests. *)
    val report : unit -> unit

    (* Assert that the given boolean is true, this is a conveniance function for
     * raising an exception on a false bool value. *)
    val assert : bool -> unit
end

structure Test :> TEST =
struct
val testsRan = ref 0
val testsPassed = ref 0
val failingTests : string list ref = ref []

exception AssertionFailed

fun test(name, f) =
  (testsRan := !testsRan + 1;
   (f();
    testsPassed := !testsPassed + 1;
    true)
   handle _ => (failingTests := name :: !failingTests; false))

fun assert test =
  if test then () else raise AssertionFailed

fun report () =
  (app print ["\n#### TEST STATISTICS ####\nPASSED: ",
              Int.toString(!testsPassed),
              "\nFAILED: ",
              Int.toString(!testsRan - !testsPassed),
              "\nTOTAL:  ",
              Int.toString(!testsRan),
              "\n#### FAILING TESTS ####\n",
              (foldl (fn (s, a) => s ^ "\n" ^ a) "" (!failingTests)),
              "\n"])

end

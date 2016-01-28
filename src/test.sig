signature TEST =
sig
  exception Failed
  val testsRan : int ref
  val testsPassed : int ref
  val test : (unit -> bool) -> bool
  val assertEq : ''a * ''a * (''a -> string) -> bool
  val assert : bool -> bool
  val reset : unit -> unit
  val printStats : unit -> unit
end

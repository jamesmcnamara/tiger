signature TEST =
sig
  exception Failed
  val testsRan : int ref
  val testsPassed : int ref
  val test : (unit -> bool) -> bool
  val assert : ''a * ''a -> bool
  val reset : unit -> unit
  val printStats : unit -> unit
end

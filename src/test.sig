signature TEST =
sig
  exception Failed

  val test : (unit -> bool) -> bool
  val assert : bool -> bool
end

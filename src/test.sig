signature TEST =
sig
  exception Error

  val test : (unit -> bool) -> bool
end

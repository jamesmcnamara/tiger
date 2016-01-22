signature TESTER =
sig
  exception Error

  val test : string -> bool
end

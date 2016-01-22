structure Test : TEST =
struct
  exception Error

  fun test f = if f () then true else false
end

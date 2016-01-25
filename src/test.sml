structure Test : TEST =
struct
  exception Failed

  fun test f = f () handle Failed => false
  fun assert b = if b then true else raise Failed
end

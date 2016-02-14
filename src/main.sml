signature MAIN =
sig
    val compile : string -> unit
end

structure Main : MAIN =
struct
    fun compile filename =
        let val exp = Parse.parse(filename)
        in
            Semant.transProg(exp)
        end
end

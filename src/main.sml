signature MAIN =
sig
    (* TODO: Should we return a unit type for now? *)
    val compile : string -> Semant.expty
end

structure Main : MAIN =
struct
    fun compile filename =
        let val exp = Parse.parse(filename)
        in
            Semant.transProg(exp)
        end
end

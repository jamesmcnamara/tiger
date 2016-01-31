structure SrcComment :> SRC_COMMENT =
struct
    type yypos = int

    val comments: yypos list ref = ref []

    fun reset () =
        comments := []

    fun start yypos =
        comments := yypos :: !comments

    fun finish yypos =
        case !comments of
            x::xs => comments := xs
          | [] => ErrorMsg.error yypos ("illegal closing comment")

    fun closed () =
        null(!comments)
end

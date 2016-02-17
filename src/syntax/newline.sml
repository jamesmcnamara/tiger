structure Newline =
struct
val lines = ref [0]

type yypos = int

fun reset () =
  lines := [0]

fun add yypos =
  lines := yypos :: !lines

fun getLine yypos =
  let fun look (p::ps, n) = if p <= yypos then n else look(ps, n-1)
      |   look _ = 0
  in
      look(!lines, List.length(!lines))
  end

fun getPos yypos =
  let fun look (p::ps) = if p <= yypos then yypos - p else look(ps)
      |   look [] = 0
  in
      look(!lines)
  end
end

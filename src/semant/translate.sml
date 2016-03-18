signature TRANSLATE =
sig
  type level
  type access
  type exp

  val outermost : level
  val newLevel : {parent: level, name: Temp.label,
                  formals: bool list} -> level
  val formals: level -> access list
  val allocLocal: level -> bool -> access
end

structure Translate : TRANSLATE = struct
  datatype level = outermost | inner of {parent: level, frame: Frame.frame}
  type access = level * Frame.access
  exception OutermostError

  fun newLevel {parent, name, formals} =
  let
    val parent = outermost
    val frame = Frame.newFrame({formals=true::formals,name=name})
  in
    inner({parent=parent, frame=frame})
  end

  fun formals level =
    case level of
        outermost => []
      | inner {parent, frame} =>
        foldl (fn (access,acc) =>
                  (inner({parent=parent, frame=frame}), access)::acc)
              []
              (Frame.formals(frame))

  fun allocLocal level escape =
    case level of
        outermost => raise OutermostError
      | inner {parent, frame} => (level, Frame.allocLocal frame escape)

  type exp = unit

end

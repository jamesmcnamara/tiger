structure Test : TEST =
struct
  exception Error

  fun test dirname =
    let
      val dir = OS.FileSys.openDir(dirname);
      val foo = ref (OS.FileSys.readDir(dir));
    in
      while (foo := OS.FileSys.readDir(dir); isSome(!foo)) do
        (print ("-> " ^ dirname ^ "/" ^ valOf (!foo) ^ "\n");
         Parse.parse(dirname ^ "/" ^ valOf (!foo)) handle ErrorMsg.Error => ());

      false
    end
end

Test.test (
    "parser smoke",
    fn () =>
       let val dir = OS.FileSys.openDir("fixtures/smoke/")
           val file = ref (OS.FileSys.readDir dir)
       in
           while isSome(!file) do (
               let val filename = "fixtures/smoke/" ^ valOf(!file);
               in
                   if OS.FileSys.isDir(filename) then
                       ()
                   else
                       (print ("-> " ^ filename ^ "\n");
                        Parse.parse(filename);
                        ())
               end
               handle Error => ();
               file := OS.FileSys.readDir(dir))
       end);

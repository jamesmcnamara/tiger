Test.test (fn () =>
    let val dir = OS.FileSys.openDir("fixtures")
        val file = ref (OS.FileSys.readDir dir)
    in
        while isSome(!file) do (
            let val filename = "fixtures/" ^ valOf(!file);
            in
                if OS.FileSys.isDir(filename) then
                    ()
                else
                    (print ("-> " ^ filename ^ "\n");
                     Parse.parseFile(filename);
                     ())
            end;
            file := OS.FileSys.readDir(dir));
        true
    end
);

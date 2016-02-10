Test.reset();

(*use "test/newline.sml";
use "test/src_string.sml";
use "test/token.sml";
use "test/lexer/comments.sml";
use "test/lexer/strings.sml";
use "test/lexer/smoke.sml";*)

use "test/parser/simple.sml";

Test.printStats();

val _ = OS.Process.exit(OS.Process.success)

Test.reset();

use "test/smoke.sml";
use "test/lexer/comments.sml";
use "test/lexer/strings.sml";
use "test/lexer/position.sml";

Test.printStats();

val _ = OS.Process.exit(OS.Process.success)

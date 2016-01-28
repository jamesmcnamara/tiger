Test.reset();

use "test/smoke.sml";
use "test/lexer/comments.sml";
use "test/lexer/strings.sml";

Test.printStats();

val _ = OS.Process.exit(OS.Process.success)

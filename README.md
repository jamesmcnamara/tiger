## Setup

Setting up things on OS X is easy thanks to Homebrew. Simple run the following
command:

```sh
$ brew install smlnj rlwrap
```

## Usage

We'll make use of the `CM` module for building the projects. To use it you
need a `sources.cm` file. Then run the following commands.

Due to a bug in MLex you currently need to manually change the generated
tiger.lex.sml to `val initPos = 1` instead of `val initPos = 2`. This only needs
to be done after cahnging the tiger.lex file.

```sh
$ rlwrap sml sources.cm
Standard ML of New Jersey v110.78 [built: Thu Aug 20 19:23:18 2015]
...

- Lexer.lexFile "fixtures/test1.tig";
```

## Parser
Members: Gary Soeller, Nathan Lilienthal, James McNamara

We encountered a few s/r conflicts with our grammer. The first two were due to
an ambiguity with our type and function declarations. The parser was unsure
whether or not it should read(shift) a new declaration or reduce after each one.
The desired action was to greedily read all of the declarations and then reduce.
By adding a precedence to each declaration list, we achieved our desired outcome.
Although we fixed one conflict, another arose regarding left brackets and ids.
This was easily fixed as we want our parser to shift when it sees a left bracket
after an id rather than reduce.

The next major s/r conflict was the dangling else problem. If we read the
statement `if 1 then if 2 then 3 else 4`, we want it to be equivalent to
`(if 1 then (if 2 then 3 else 4))`. This was solved by setting the precedence
of else to be higher than if making it bind to the nearest if.

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

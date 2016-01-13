## Setup

Setting up things on OS X is easy thanks to Homebrew. Simple run the following
command:

```sh
$ brew install smlnj rlwrap
```

## Usage

We'll make use of the `CM` module for building the projects. To use it you
need a `sources.cm` file. Then run the following commands.


```sh
$ rlwrap sml
Standard ML of New Jersey v110.78 [built: Thu Aug 20 19:23:18 2015]
- CM.make "sources.cm";
...
val it = true : bool

- Parse.parse "fixtures/test1.tig";
```

## Coding Conventions

- Use 4 spaces for basic indentation, and **never** use tabs.

# Compilers - CS4110

- Gary Soeller
- Nathan Lilienthal
- James McNamara

Writeups are included for each section of the compiler:

- [Lexer](#lexer)
- [Parser](#parser)

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
$ rlwrap sml sources.cm tests.sml
```

## Lexer

We handled comments by creating a structure which acts as a stack containing
line positions where comments were opened. As the lexer sees a `/*` string in
the `initial` state, it adds the line position to the stack. When it sees a
`*/` string in the `comment` state, it pops a position from the stack. Using a
stack in this way, we are able to tell if all of the comments are eventually
closed or not.

Our error handling for strings and comments was done in the EOF function. In a
similar way to how we had a structure to handle comments, we also had one to
handle strings. In the EOF function, we checked the state of both of these
structures to ensure comments and strings were fully closed. If one of them is
not, we print an error and throw an appropriate exception. If all comments and
strings are closed, we simply return the EOF token.

One aspect of our code which we think will be of great importance as we progress
through the compiler is how we tested our code. We developed our own test
structure which allows us to run tests in a repeatable manner. Our testing code
includes statistics on how many tests pass or fail as well as verbose output
when a test fails.

Another interesting aspect of our compiler is that we use a lookup table to
manage ids and keywords. Using this lookup table, we are able to easily map
strings to reserved keywords and seen ids.

## Parser

We encountered a few s/r conflicts with our grammar. The first two were due to
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

# Bruijn

A purely academic programming language based on lambda calculus and De
Bruijn indices written in Haskell.

[Jump to examples](#Examples)

## Features

-   De Bruijn indices[\[0\]](#References) eliminate the complexity of
    α-equivalence and α-conversion
-   Unique bracket-style representation for lambda abstractions enables
    improved human-readability and faster syntactic perception
-   Balanced ternary allows negative numbers while having a reasonably
    compact representation, operator and time complexity (in comparison
    to unary/binary church numerals)[\[1\]](#References)
-   Arbitrary-precision floating-point artihmetic using balanced ternary
    numerals
-   Highly space-efficient compilation to binary lambda calculus
    (BLC)[\[2\]](#References)[\[3\]](#References) additionally to normal
    interpretation and REPL
-   Use BLC compilation in combination with generative asymmetric
    numeral systems (ANS/FSE)[\[4\]](#References) as incredibly
    effective compressor
-   Contracts as a form of typing because typing while guaranteeing
    turing-completeness isn’t a trivial
    [problem](https://cstheory.stackexchange.com/a/31321) in LC
-   Strongly opinionated parser with strict syntax rules
-   Recursion can be implemented using combinators such as Y, Z or ω
-   Included standard library featuring many useful functions (see
    `std/`)

## Basics

### De Bruijn indices

De Bruijn indices[\[0\]](#References) replace the concept of variables
in lambda calculus. The index basically represents the abstraction layer
you want to reference beginning at 0 with the innermost layer.

For example, λx.x becomes λ0 because x referenced the first abstraction
layer. Furthermore, λx.λy.xy becomes λλ10 and so forth.

You can read more about De Bruijn indices on
[Wikipedia](https://en.wikipedia.org/wiki/De_Bruijn_index).

### Syntax

In general the syntax of bruijn is pretty similar to the previously
presented normal lambda calculus syntax with De Bruijn indices.

You can use any function that you’ve previously defined. You can also
overwrite previously defined functions. The environment gets interpreted
from bottom to top (starting at `main`).

The following are the main syntax specifications in the (minorly
extended) [Backus-Naur
form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form).

    <identifier>  ::= [a-ω,A-Ω,_][a-ω,A-Ω,0-9,?,!,',-]*
    <namespace>   ::= [A-Ω][a-ω,A-Ω]+
    <abstraction> ::= "[" <expression> "]"
    <numeral>     ::= ("+" | "-")[0-9]+
    <bruijn>      ::= [0-9]
    <singleton>   ::= <bruijn> | <numeral> | <abstraction> | "(" <application> ")" | [namespace.]<identifier>
    <application> ::= <singleton> <singleton>
    <expression>  ::= <application> | <singleton>
    <test>        ::= ":test " "(" <expression> ") (" <expression> ")"
    <import>      ::= ":import " <path> [namespace]
    <comment>     ::= "# " <letter>*

The following are the differences in syntax between REPL and file:

**For files**:

The execution of a file begins at the `main` function. Its existence is
mandatory.

    <print>      ::= ":print " <expression>
    <definition> ::= <identifier> <expression>
    <line>       ::= <definition> | <print> | <comment> | <import> | <test> | "\n"

**For REPL**:

    <definition> ::= <identifier> = <expression>
    <line>       ::= <definition> | <expression> | <comment> | <import> | <test> | "\n"

### Numerals

Numbers in bruijn always have a sign in front of them or else they will
be mistaken for De Bruijn indices. They also need to be between
parenthesis because of prefix functions. Generally the decimal
representation is only syntactic sugar for its internal balanced ternary
representation. We use balanced ternary because it’s a great compromise
between performance and size (according to [\[1\]](#References)).

You don’t have to care about the internals too much though as long as
you use the included operations from the standard library. The REPL even
tries its best at displaying expressions that look like ternary numbers
as decimal numbers in paranthesis next to it.

### Standard library

You may want to use the included standard library to reach your
program’s full potential. It includes many common combinators as well as
functions for numerical, boolean and IO operations and much more.

For example, you can import the standard library for numbers using
`:import std/Number`. You can find all available libraries in the `std/`
directory.

### Examples

You can try these by experimenting in the REPL or by running them as a
file. You should pipe something into the stdin to receive stdout:
`cat /dev/null | bruijn test.bruijn` should work for now.

**Remember** that you need an equal sign between the function name and
its definition if you’re using the REPL.

#### Plain execution without any predefined functions

Without using its standard library bruijn is basically unmodified, pure
lambda calculus with syntactically sugared balanced ternary numerals,
string and chars. Bruijn doesn’t support any numerical operations or any
other infix/prefix functions by default. Using it without its standard
library can be quite fun, though - especially for exploring and
understanding the logic of lambda calculus:

    # this is a comment
    # we now define a function returning a ternary 1
    get-one (+1)

    # we can use the function in all functions below its definition
    get-one2 get-one

    # tests are similar to assertions in other languages
    # they test equality using α-equivalence of reduced expressions
    # in this example they're used to show the reduced expressions
    :test (get-one2) ((+1))

    # remember that numbers always need to be written in parenthesis
    # therefore two braces are needed in tests because testing exprs
    # must always be in parenthesis as well

    # indenting acts similarly to Haskell's where statement
    get-one3 foo
        bar (+1)
        foo bar

    # equivalent of λx.x or Haskell's id x = x
    id [0]

    # testing equivalent of (λx.x) (λx.λy.x) = λx.λy.x
    :test (id [[1]]) ([[1]])

    # prefix function definition
    !( [[1]]

    # use prefix function '!'
    # ![0] becomes ([[1]] [0]) which in turn becomes [[0]]
    :test (![0]) ([[0]])

    # infix function definition: flip and apply arguments
    (<>) [[0 1]]

    # use infix function '<>'
    # [[0]] <> [[1]] becomes (([[0 1]] [[0]]) [[1]])
    :test ([[0]] <> [[1]]) ([[1]] [[0]])

    # multiple arguments
    number-set set-of-three (+1) (+2) (+3)
        set-of-three [[[[0 1 2 3]]]]

    access-first [0 [[[0]]]]

    :test (access-first number-set) ((+1))

    # ignore args and return string 
    main ["Hello world!\n"]

#### Using standard library

Concatenating “Hello world” program using IO:

    :import std/List .

    main [("Hello " ++ 0) ++ "!\n"]

You can then use `printf "world" | bruijn file.bruijn` to get “Hello
world!”

Some other great functions:

    :import std/Logic .
    :import std/Combinator .
    :import std/Number .
    :import std/Option .
    :import std/Pair .
    :import std/List .

    # pairs with some values
    love pair me you
        me [[[1]]]
        you [[[2]]]

    :test (fst love) ([[[1]]])
    :test (snd love) ([[[2]]])

    # you can also write (me : you) instead of (pair me you)
    # also (^love) and (~love) instead of (fst love) and (snd love)

    # numerical operations
    five --(((+8) + (-4)) - (-2))

    not-five? [if (0 =? (+5)) false true]

    :test (not-five? five) (false)

    :test ((uncurry mul (pair (+3) (+2))) =? (+6)) (true)

    # lazy evaluation using infinite lists and indexing
    pow2 [(iterate (mul (+2)) (+1)) !! 0]

    :test (pow2 (+5)) ((+32))

    # options
    :test (map inc (some (+1))) (some (+2))
    :test (apply (some (+1)) [some (inc 0)]) (some (+2))

    # boolean
    main not ((false && true) || true)

    :test (main) (false)

Read the files in std/ for an overview of all functions/libraries.

### Compilation to BLC

You can compile bruijn to John Tromp’s
BLC[\[2\]](#References)[\[3\]](#References). Only the used functions
actually get compiled in order to achieve a minimal binary size.

BLC uses the following encoding:

| term         | lambda | bruijn | BLC              |
|:-------------|:-------|:-------|:-----------------|
| abstraction  | λM     | \[M\]  | 00M              |
| application  | MN     | MN     | 01MN             |
| bruijn index | i      | i      | 1<sup>i+1</sup>0 |

## Installation

You first need to install Haskell and Haskell Stack using the guidelines
of your operating system.

Using Haskell Stack, run `stack run -- [args]` to play around and use
`stack install` to install bruijn into your path.

## REPL config

You can configure the REPL by editing the `config` file. `stack install`
or `stack run` will move the file into a data directory.

More options can be found
[here](https://github.com/judah/haskeline/wiki/UserPreferences).

## Usage

Please read the usage information in the executable by using the `-h`
argument.

## References

0.  De Bruijn, Nicolaas Govert. “Lambda calculus notation with nameless
    dummies, a tool for automatic formula manipulation, with application
    to the Church-Rosser theorem.” Indagationes Mathematicae
    (Proceedings). Vol. 75. No. 5. North-Holland, 1972.
1.  Mogensen, Torben. “An investigation of compact and efficient number
    representations in the pure lambda calculus.” International Andrei
    Ershov Memorial Conference on Perspectives of System Informatics.
    Springer, Berlin, Heidelberg, 2001.
2.  Tromp, John. “Binary lambda calculus and combinatory logic.”
    Randomness and Complexity, from Leibniz to Chaitin. 2007. 237-260.
3.  Tromp, John. “Functional Bits: Lambda Calculus based Algorithmic
    Information Theory.” (2022).
4.  Duda, Jarek. “Asymmetric numeral systems.” arXiv preprint
    arXiv:0902.0271 (2009).

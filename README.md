# Bruijn

A purely academic programming language based on lambda calculus and De
Bruijn indices written in Haskell.

## Features

-   De Bruijn indices\[0\] eliminate the complexity of α-equivalence and
    α-conversion
-   Unique bracket-style representation for lambda abstractions enables
    improved human-readability and faster syntactic perception
-   Balanced ternary allows negative numbers while having a reasonably
    compact representation, operator and time complexity (in comparison
    to unary/binary church numerals)\[1\]
-   Highly space-efficient compilation to binary lambda calculus
    (BLC)\[2\]\[3\] additionally to normal interpretation and REPL
-   Recursion can be implemented using combinators such as Y or ω
-   Included standard library featuring many useful functions
    (`std.bruijn`)

## Basics

### De Bruijn indices

De Bruijn indices\[0\] replace the concept of variables in lambda
calculus. The index basically represents the abstraction layer you want
to reference beginning at 0 with the innermost layer.

For example, λx.x becomes λ0 because x referenced the first abstraction
layer. Furthermore, λx.λy.xy becomes λλ10 and so forth.

You can read more about De Bruijn indices on
[Wikipedia](https://en.wikipedia.org/wiki/De_Bruijn_index).

### Syntax

In general the syntax is pretty similar to the previously presented
normal lambda calculus syntax with De Bruijn indices.

You can use any function that you’ve previously defined. You can also
overwrite previously defined functions. The environment gets interpreted
from down to top.

The following are the main syntax specifications in the (minorly
extended) [Backus-Naur
form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form). Spaces
are optional but allowed.

    <identifier>  ::= [a-ω,A-Ω,_][a-ω,A-Ω,0-9,?,!,',_]*
    <abstraction> ::= "[" <expression> "]"
    <numeral>     ::= ("+" | "-")[0-9]+
    <bruijn>      ::= [0-9]
    <singleton>   ::= <bruijn> | <numeral> | <abstraction> | "(" <application> ")" | <identifier>
    <application> ::= <singleton> <singleton>
    <expression>  ::= <application> | <singleton>
    <test>        ::= ":test " <expression> = <expression>
    <import>      ::= ":import " <path>
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

### Standard library

You may want to use the included standard library to reach your
program’s full potential. It includes many common combinators as well as
functions for numerical, boolean and IO operations and much more.

You can import it from `std.bruijn` using `:import std`.

### Examples

Plain execution

    # equivalent of λx.x
    id [0]

    # equivalent of (λx.x) (λx.λy.x) = λx.λy.x
    :test id [[1]] = [[1]]

    # endless loop using omega combinator
    om [0 0]
    nom om
    main om nom

Using standard library

    :import std

    # equivalent of (λx.x) (λx.λy.x) = λx.λy.x
    outer id [[1]]
    :test outer = [[1]]

    # pairs
    me [[[1]]]
    you [[[2]]]
    love pair me you
    :test fst love = me
    :test snd love = you

    # boolean
    main not (or (and false true) true)
    :test main = [0]

### Compilation to BLC

You can compile bruijn to John Tromp’s BLC\[2\]\[3\]. Only the used
functions actually get compiled in order to achieve a minimal binary
size.

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

## Usage

Please read the usage information in the executable by using the `-h`
argument.

## References

-   \[0\] De Bruijn, Nicolaas Govert. “Lambda calculus notation with
    nameless dummies, a tool for automatic formula manipulation, with
    application to the Church-Rosser theorem.” Indagationes Mathematicae
    (Proceedings). Vol. 75. No. 5. North-Holland, 1972.
-   \[1\] Mogensen, Torben. “An investigation of compact and efficient
    number representations in the pure lambda calculus.” International
    Andrei Ershov Memorial Conference on Perspectives of System
    Informatics. Springer, Berlin, Heidelberg, 2001.
-   \[2\] Tromp, John. “Binary lambda calculus and combinatory logic.”
    Randomness and Complexity, from Leibniz to Chaitin. 2007. 237-260.
-   \[3\] Tromp, John. “Functional Bits: Lambda Calculus based
    Algorithmic Information Theory.” (2022).

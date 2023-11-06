# Syntax

Bruijn has an arguably weird syntax, although it's not strictly meant as
an esoteric programming language. Most notably the usage of lambda
calculus logic, combinators, and De Bruijn indices can be confusing at
first -- it's definitely possible to get used to them though!

Bruijn uses a [variation of lambda calculus](lambda-calculus.md). For
subjective syntax rules, read [coding style](../coding/style.md). Also
see the [examples](../coding/examples.md).

## Definitions

Bruijn works by defining named substitution rules, where each usage of
the identifier will get substituted respectively. You can't use
definitions before they are defined, every file gets substituted from
top to bottom.

Since there's no other major syntactic block, bruijn omits the equal
symbol for definitions.

For example:

``` bruijn
# define `id` as [0]
id [0]

# [0] [0] ⤳ [0]
still-id id id

# `main` is now [0]
main still-id
```

Note that this does *not* actually extend the functionality of lambda
calculus. These identifiers are static constants and can't be
dynamically changed.

In fact, bruijn's interpreter works by producing *one* final huge lambda
calculus expression for any given program, that then gets reduced by a
normal lambda calculus reducer.

## Open terms

If you use De Bruijn indices that reach out of their environment, you
have created an *open term*. Depending on the context, these terms are
typically seen as invalid if standing by themself.

``` bruijn
# open terms
open0 0
open1 [[2]]
open2 [[0 1] 1]

# closed terms
closed0 [0 [1]]
closed1 [[[2 0] 1]]
```

Bruijn does not give warnings for open terms and reduces them as normal.
In some cases it's actually encouraged to use open terms as sub-terms
for improved readability (see [coding style
suggestions](../coding/style.md)).

## Imports

Files can either be imported into a namespace (capital word) or the
current environment (.):

``` bruijn
:import std/Pair P
:import std/Logic .

main [P.pair true false]
```

All paths get the `.bruijn` extension appended automatically.

Only top-level definitions get imported using `:import`{.bruijn}. If you
also want to import second-level definitions (for example imported
definitions from the imported file), you can use `:input`{.bruijn}.

``` bruijn
:input std/Math
```

## Tests

Tests compare the *normal form* of two expressions. Note that the
parentheses around the two terms are always mandatory.

``` bruijn
:test ([0] [[1]]) ([[1]] [[1]] [0])
```

Execution succeeds silently. Example of failing test:

``` bruijn
:test ([0] [[1]]) ([[1]] [0])
```

After running:

    ERROR test failed: ([0] [[1]]) = ([[1]] [0])
          reduced to [[1]] = [[0]]

Tests are always run for the executed file and any files it contains. If
they take too long and you're sure your code is correct, you can enable
the *YOLO* mode to disable tests by using bruijn's `-y` argument.

## Scoping

Indented lines (by tab) act as a `where` statement for the less indented
lines.

``` bruijn
foo [[0]]

bar [0] foo
    foo bar
        bar [[1]]

# foo is still `[[0]]`
# bar is `[0] [[1]]`
main [[0 foo bar]]
```

Also note that bruijn does *not* support recursion -- you wouldn't be
able to use `bar`{.bruijn} in `foo`{.bruijn} without its sub definition.
See [recursion](../coding/recursion.md) to learn how to use recursion
anyway.

## Syntactic sugar

Some typical data encodings are provided as syntactic sugar. You can
learn more about the internal specifics in [data
structures](../coding/data-structures.md).

-   *Numbers*: `(SXB)`{.bruijn} where `S` is `+`/`-`, `X` is a number
    and `B` is the *base* of the encoded number (or `t` by default)
    -   `u` for unary base (postive, Church): `(+42u)`
    -   `b` for binary base (positive): `(+42b)`
    -   `t` for balanced ternary (positive/negative): `(-42t)`
-   *Characters*: `'C'`{.bruijn} where `C` is any ASCII character
-   *Strings*: `"X1..XN"`{.bruijn} where `X1...XN` are any ASCII
    characters
-   *Quotes*: `` `T ``{.briujn} where `T` is any lambda term ([meta
    programming](../coding/meta-programming.md))

## Types

As of right now, typing is entirely optional and *purely* for
documentation/aesthetics. Aside from syntax, types do not get checked in
any way.

We do have plans to implement type checking in the future, unfortunately
almost all trivial typing mechanisms for pure lambda calculus reduce its
power immensely.

The current syntax of types is quite simple:

    POLYMORPHIC := [a-z]+
    CONSTRUCTOR := ([A-Z][a-z]* TYPE)
    FUNCTION    := (TYPE → ... → TYPE)
    IDENTIFIER  := [A-Z][a-z]*
    TYPE        := IDENTIFIER | TYPE-VARIABLE | CONSTRUCTOR | FUNCTION
    SIGNATURE   := TYPE → ... → Type

The type signature can be written at the end of any bruijn term using
the `⧗`{.bruijn} symbol.

Examples:

``` bruijn
# from std/Combinator
c [[[2 0 1]]] ⧗ (a → b → c) → b → a → c

# from std/List
empty? [0 [[[false]]] true] ⧗ (List a) → Boolean
```

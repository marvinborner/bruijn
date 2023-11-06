# Examples

## Hello world!

Hello world using [lists](../coding/data-structures.md#lists-stdlist)
and [IO](../coding/IO.md)!

``` bruijn
:import std/List .

main ["Hello " ++ 0 ++ "!\n"]
```

``` bash
$ printf "world" | bruijn file.bruijn
Hello world!
```

## Syntax

Example functions demonstrating the syntax without usage of
[`std/`](/std/).

``` bruijn
# this is a comment
# returns ternary 1 (syntactic sugar)
get-one (+1)

# we can use the function in all functions below its definition
get-one2 get-one

# tests are similar to assertions in other languages
# they test equality using α-equivalence of reduced expressions
:test (get-one2) ((+1))

# indenting acts similarly to Haskell's where statement
get-one3 foo
    bar (+1)
    foo bar

# equivalent of λx.x or Haskell's id x = x
id [0]

# testing equivalent of (λx.x) (λx.λy.x) = λx.λy.x
# the numbers in the abstractions refer to arguments using
# De Bruijn indices
:test (id [[1]]) ([[1]])

# prefix function definition
!‣ [[1]]

# use prefix function '!'
# ![0] becomes ([[1]] [0]) which in turn becomes [[0]]
:test (![0]) ([[0]])

# infix function definition: flip and apply arguments
…<>… [[0 1]]

# use infix function '<>'
# [[0]] <> [[1]] becomes (([[0 1]] [[0]]) [[1]])
:test ([[0]] <> [[1]]) ([[1]] [[0]])

# multiple arguments
number-set set-of-three (+1) (+2) (+3)
    set-of-three [[[[0 1 2 3]]]]

access-first [0 [[[0]]]]

:test (access-first number-set) ((+1))

# ignore stdin and return string
main ["Hello world!\n"]
```

## Standard library

``` bruijn
:import std/Combinator .
:import std/List .
:import std/Logic .
:import std/Number .
:import std/Option .
:import std/Pair .

# pairs with some values
love pair me you
    me [[[1]]]
    you [[[2]]]

:test (fst love) ([[[1]]])
:test (snd love) ([[[2]]])

# you can also write (me : you) instead of (pair me you)
# also ^love and ~love instead of (fst love) and (snd love)

# numerical operations
# remember that every mixfix chain is left-associative
five --((+8) + (-4) - (-2))

not-five? [if (0 =? (+5)) false true]

# awesome mixfix functions
:test (∑ (+1) → (+3) | [++0]) ((+9))
:test (∏ (+1) → (+3) | [++0]) ((+24))

:test (not-five? five) (false)

:test ((uncurry mul (pair (+3) (+2))) =? (+6)) (true)

# lazy evaluation using infinite lists and indexing
pow2 …!!… (iterate (…⋅… (+2)) (+1))

:test ((pow2 (+5)) =? (+32)) (true)

# options
:test (map inc (some (+1))) (some (+2))
:test (apply (some (+1)) [some ++0]) (some (+2))

# boolean
# the main function gets executed automatically
# ignore stdin arguments by not referencing 0
main [¬(false ⋀? true ⋁? true)]

:test (main [0]) (false)
```

## More examples

You can find more example programs in
[`samples/`](https://github.com/marvinborner/bruijn/tree/main/samples)
of our source-code repository. The samples include several solutions to
[Advent of Code](https://adventofcode.com/) problems.

Reading the source of the [standard library](/std/) can also be helpful.

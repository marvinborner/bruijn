# Coding style

## Program

-   Every function has to be delimited by one empty line.
-   Every (non-scoped) function must have a comment directly above it.
-   Tests must appear in a single block (no empty lines) one line under
    the definition.

See the [standard library](/std/) for inspiration.

## Function naming

De Bruijn indices can be seen as a disadvantage to readability. It's
therefore much more important to name the functions appropriately.

For functions that return a boolean, we suggest using the suffix `?`. If
your function has different cases it's recommended to use the `case-`
prefix in scoped sub-terms.

``` bruijn
# from std/Ternary
zero? [0 case-end case-neg case-pos i] ⧗ Number → Boolean
    case-end true
    case-neg [false]
    case-pos [false]
```

Appropriate [type signatures](../introduction/syntax.md#types) are also
encouraged.

## If/else

Since booleans are just lambda terms either returning its first or
second argument, the use of if/else procedures is generally redundant.
See [bit/boolean data
structure](data-structures.md#booleansbits-stdlogic).

``` bruijn
:test (true 'a' 'b') ('a')
:test (false 'a' 'b') ('b')
```

## Head/tail

The internal structure of the list encoding means that when a list is
applied to a function, the function is called with the head and tail of
the list.

``` bruijn
:test ("abc" [[1]]) ('a')
:test ("abc" [[0]]) ("bc")
```

Therefore the recommended style for coding with lists is to use
`head`{.bruijn}/`tail`{.bruijn} only when truly needed.

## Recursion

[Recursion](recursion.md) should almost always be achieved with the
`y`{.bruijn} or `z`{.bruijn} combinators.

A common coding style in bruijn's standard library is to use the scoped
`rec` function to indicate recursion. You would then use `n+1`
abstraction around `rec` to indicate `n` arguments and the additionally
induced recursive call.

Example of the `length`{.bruijn} function for lists:

``` bruijn
# 3 abstractions => two arguments
# 2 is recursive call
# 1 is accumulator (+0)
# 0 is argument (list)
length z [[[rec]]] (+0) ⧗ (List a) → Number
    rec 0 [[[case-inc]]] case-end
        case-inc 5 ++4 1
        case-end 1
```

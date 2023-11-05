# Coding style

## Scoping

## If/else

redundant

## Head/tail

redundant

## Type signatures

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

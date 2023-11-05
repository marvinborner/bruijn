# Test driven development (TDD)

The suggested technique for bruijn development is the TDD method. When
creating functions, we suggest the following procedure:

-   Write a comment, a type signature, and the head of the function

``` bruijn
# measures the length of a list
length y [[[rec]]] (+0) ⧗ (List a) → Number
```

-   Write several tests including all edge cases

``` bruijn
# measures the length of a list
length y [[[rec]]] (+0) ⧗ (List a) → Number

:test (length empty) ((+0))
:test (length "a") ((+1))
:test (length ({}empty)) ((+1))
:test (length (empty : {}empty)) ((+2))
:test (length ("abc")) ((+3))
```

-   Finish the implementation until all tests pass (e.g. using the
    [`:watch`{.bruijn} repl command](REPL.md#watch))
-   Refactor and clean up the definition

``` bruijn
# measures the length of a list
length y [[[rec]]] (+0) ⧗ (List a) → Number
    rec 0 [[[case-inc]]] case-end
        case-inc 5 ++4 1
        case-end 1

:test (length empty) ((+0))
:test (length "a") ((+1))
:test (length ({}empty)) ((+1))
:test (length (empty : {}empty)) ((+2))
:test (length ("abc")) ((+3))
```

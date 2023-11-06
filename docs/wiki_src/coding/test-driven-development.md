# Test driven development (TDD)

The suggested technique for bruijn development is the TDD method. When
creating functions, we suggest the following procedure:

-   Write a comment, a type signature, and the head of the function

``` bruijn
# returns the item at index in a list, starting from 0
index y [[[rec]]] ⧗ (List a) → Number → a
```

-   Write several tests including edge cases

``` bruijn
# returns the item at index in a list, starting from 0
index y [[[rec]]] ⧗ (List a) → Number → a

:test (empty !! (+0)) (empty)
:test ({}(+1) !! (+0)) ((+1))
:test (((+1) : ((+2) : {}(+3))) !! (+0)) ((+1))
:test (((+1) : ((+2) : {}(+3))) !! (+2)) ((+3))
:test (((+1) : ((+2) : {}(+3))) !! (-1)) (empty)
:test (((+1) : ((+2) : {}(+3))) !! (+3)) (empty)
```

-   Finish the implementation until all tests pass (e.g. using the
    [`:watch`{.bruijn} repl command](REPL.md#watch))
-   Refactor and clean up the definition

``` bruijn
# returns the item at index in a list, starting from 0
index y [[[rec]]] ⧗ (List a) → Number → a
    rec 0 [[[case-index]]] case-end
        case-index =?4 2 (5 --4 1)
        case-end empty

:test (empty !! (+0)) (empty)
:test ({}(+1) !! (+0)) ((+1))
:test (((+1) : ((+2) : {}(+3))) !! (+0)) ((+1))
:test (((+1) : ((+2) : {}(+3))) !! (+2)) ((+3))
:test (((+1) : ((+2) : {}(+3))) !! (-1)) (empty)
:test (((+1) : ((+2) : {}(+3))) !! (+3)) (empty)
```

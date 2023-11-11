# Meta programming

Bruijn has a homoiconic meta encoding inspired by Lisp's quoting
feature.

Blog post with more details: [Metaprogramming and
self-interpretation](https://text.marvinborner.de/2023-09-03-21.html).

## Encoding

``` bruijn
`X     ⤳ [[[2 (+Xu)]]]
`(M N) ⤳ [[[1 `M `N]]]
`[M]   ⤳ [[[0 `M]]]
```

Any quoted term gets converted to this encoding:

``` bruijn
# example quotations
:test (`0) ([[[2 (+0u)]]])
:test (`[0]) ([[[0 [[[2 (+0u)]]]]]])
:test (`'0') ([[[0 [[[0 [[[0 [[[1 [[[2 (+0u)]]] [[[1 [[[2 (+0u)]]] [[[1 [[[2 (+0u)]]] [[[1 [[[2 (+0u)]]] [[[1 [[[2 (+1u)]]] [[[1 [[[2 (+1u)]]] [[[1 [[[2 (+0u)]]] [[[1 [[[2 (+0u)]]] [[[2 (+2u)]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]])

# quotes are nestable!
:test (``0) ([[[0 [[[0 [[[0 [[[1 [[[2 (+2u)]]] [[[0 [[[0 [[[2 (+0u)]]]]]]]]]]]]]]]]]]]]])
:test (`[0 `0]) ([[[0 [[[1 [[[2 (+0u)]]] [[[0 [[[0 [[[0 [[[1 [[[2 (+2u)]]] [[[0 [[[0 [[[2 (+0u)]]]]]]]]]]]]]]]]]]]]]]]]]]])
```

## Quasiquotation

Quoted terms can be escaped (*unquoted*) using the comma symbol.
Unquoted terms will be fully evaluated first before getting quoted
again.

``` bruijn
:test (```,[0]) (``[0])
:test (`,`,[0]) ([0])
:test (`[0 `,[0]]) (`[0 [0]])
```

Unquoted De Bruijn indices will get bound to the respective abstraction
outside of its meta encoding.

``` bruijn
# adds two using normal quotation
add-two `[0 + (+2u)]

:test (!add-two (+2u)) ((+4u))

# adds two using a reaching De Bruijn index
add-two* [`(,0 + (+2u))]

:test (!(add-two* `(+2u))) ((+4u))
```

## Meta library [`std/Meta`](/std/Meta.bruijn.html)

The meta library enables simple interaction with the meta encoding.

Examples:

``` bruijn
# testing equivalence
:test (α-eq? `[0 0] `[0 0]) (true)
:test (α-eq? `α-eq? `α-eq?) (true)

# BLC length of meta term
:test (length `[0]) ((+4u))
:test (length `[[1 1]]) ((+12u))

# self-modification
:test (lhs `(1 0) `0) (`(0 0))
:test (rhs `(0 1) `0) (`(0 0))
:test (swap `(1 0)) (`(0 1))
:test (map inc `0) (`1)
:test (map (map inc) `[0]) (`[1])
:test (map swap `[0 1]) (`[1 0])
```

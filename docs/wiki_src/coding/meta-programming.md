# Metaprogramming

Bruijn has a homoiconic meta encoding inspired by Lisp's quoting
feature.

Blog post with more details: [Metaprogramming and
self-interpretation](https://text.marvinborner.de/2023-09-03-21.html).

## Encoding

    `X     ⤳ [[[2 (+Xu)]]]
    `(M N) ⤳ [[[1 `M `N]]]
    `[M]   ⤳ [[[0 `M]]]

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

Unquoted de Bruijn indices will get bound to the respective abstraction
outside of its meta encoding.

``` bruijn
# adds two using normal quotation
add-two `[0 + (+2u)]

:test (!add-two (+2u)) ((+4u))

# adds two using a reaching de Bruijn index
add-two* [`(,0 + (+2u))]

:test (!(add-two* `(+2u))) ((+4u))
```

## Self-interpretation

Using a metacircular self-interpreter, bruijn can reduce the meta
encoding to its normal form. A 194 bit interpreter in the form of
bruijn's logo:

``` code-showcase
01010001                                    00011100
11010000               ######               11100110
10000               ############               00001
01011              #####    #####              00001
11100             ####        ####             00101
01110             ####       #####             00011
00000             ####      ######             10100
00011             ####    ### ####             00111
10000             ####   ##   ####             11111
00001             #### ###    ####             11110
00010             ######      ####             11110
10011             #####       ####             10100
11110             ####        ####             00011
11000              #####    #####              00011
11000               ############               01011
01101110               ######               00011001
00011010                                    00011010
```

The code can also be found in the `eval`{.bruijn}/`!‣`{.bruijn} function
of the meta library:

``` bruijn
:test (!`"tacocat".reverse) ("tacocat")
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

# encoding terms as numbers
:test ((encode `(0 0)) =? (+3)) (true)
:test ((encode `[0]) =? (+8)) (true)

# decoding numbers to terms
:test (decode (+3)) (`(0 0))
:test (decode (+8)) (`[0])
```

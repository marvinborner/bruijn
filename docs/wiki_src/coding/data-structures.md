# Data structures

Bruijn's standard library defines several common data structures.

Relevant blog post: [Data structures in pure lambda
calculus](https://text.marvinborner.de/2023-04-07-01.html).

## States

For storing states (i.e. enums), you can use the available libraries and
syntactic sugar.

### Booleans/bits [`std/Logic`](/std/Logic.bruijn.html)

-   typical [Church
    booleans](https://en.wikipedia.org/wiki/Church_encoding#Church_Booleans)
    -- fast and reliable
-   encoding: `true`{.bruijn}=`[[1]]`{.bruijn},
    `false`{.bruijn}=`[[0]]`{.bruijn}

### Unary numbers [`std/Number/Unary`](/std/Number_Unary.bruijn.html)

-   `u` suffix for syntactic sugar, e.g. `(+3u)`{.bruijn}
-   encoding: `(+4u)`{.bruijn}=`[[(1 (1 (1 (1 0))))]]`{.bruijn}
-   typical [Church
    numerals](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals),
    simple but high space/time complexity
-   only positive numbers

### Binary numbers [`std/Number/Binary`](/std/Number_Binary.bruijn.html)

-   `b` suffix for syntactic sugar, e.g. `(+3b)`{.bruijn}
-   encoding: `(+4b)`{.bruijn}=`[[[0 (0 (1 2))]]]`{.bruijn}
-   encoding for chars/strings, e.g. `'0'`{.bruijn}=`(+48b)`{.bruijn}
-   faster and more compact than unary
-   only positive numbers (excluding two's complement)

### Balanced ternary [`std/Number/Ternary`](/std/Number_Ternary.bruijn.html)

-   default syntactic sugar for numbers (optional suffix `t`),
    e.g. `(+3)`{.bruijn}
-   encoding: `(+4)`{.bruijn}=`[[[[(1 (1 3))]]]]`{.bruijn},
    `(-4)`{.bruijn}=`[[[[(2 (2 3))]]]]`{.bruijn}
-   faster and more compact than binary[^1]
-   positive and negative numbers

## Boxes [`std/Box`](/std/Box.bruijn.html)

Boxes are good for storing single values as immutable object with an
empty/full state.

Example:

``` bruijn
a-box <>'a'

:test (set? a-box) (true)
:test (get 'b' a-box) ('a')
:test (get 'b' empty) ('b')
:test (store! a-box 'b') (<>'b')
```

Options ([`std/Option`](/std/Option.bruijn.html)) use the same data
structure and have additional definitions to resemble Haskell's
`Maybe`{.haskell}.

## Pairs [`std/Pair`](/std/Pair.bruijn.html)

Pairs (tuples) can store any two terms. Pairs can be constructed using
the `…:…`{.bruijn} [mixfix](mixfix.md) function.

Example:

``` bruijn
one-two (+1) : (+2)

:test (^one-two) ((+1))
:test (~one-two) ((+2))
:test (inc <$> one-two) ((+2) : (+3))
:test (uncurry add one-two) ((+3))
```

## Lists [`std/List`](/std/List.bruijn.html)

Lists are a repeated composition (right-associative) of pairs with a
`empty`{.bruijn} ending symbol `[[1]]`{.bruijn}. They can store any
(heterogeneous) values and are recursively iterable. The call-by-need
reduction order of bruijn allows lazy evaluation (i.e. infinite lists).

Due to the right-associativeness, writing lists by hand is slightly
annoying. The usage of the `…:…`{.bruijn} [mixfix](mixfix.md) and
`{}‣`{.bruijn} [prefix](prefix.md) functions to denote pairs and the
final `empty`{.bruijn} symbol is encouraged.

Example:

``` bruijn
:test (length (take (+3) (repeat (+4)))) ((+3))
:test (take (+5) (iterate ++‣ (+0))) (((+0) : ((+1) : ((+2) : ((+3) : {}(+4))))))
:test ((foldr …+… (+0) ((+1) : ((+2) : {}(+3)))) =? (+6)) (true)
```

The internal structure of the list encoding means that when a list is
applied to a function, the function is called with the head and tail of
the list.

``` bruijn
:test ("abc" [[1]]) ('a')
:test ("abc" [[0]]) ("bc")
```

## Strings [`std/String`](/std/String.bruijn.html)

Strings are just a list of binary encoded bytes. You may use
[`std/List`](/std/List.bruijn.html) in combination with
[`std/Number/Binary`](/std/Binary.bruijn.html) to interact with them.

Example:

``` bruijn
:test (lines "abc\ndef") ("abc" : {}"def")
:test ("ab" =? "ab") (true)
```

[^1]: [Mogensen, Torben Æ. "An investigation of compact and efficient
    number representations in the pure lambda calculus." Perspectives of
    System Informatics: 4th International Andrei Ershov Memorial
    Conference, PSI 2001 Akademgorodok, Novosibirsk, Russia, July 2--6,
    2001 Revised Papers 4. Springer Berlin Heidelberg,
    2001.](https://doi.org/10.1007/3-540-45575-2_20)

# IO

Bruijn supports a variant of John Tromp's monadic IO[^1].

Every program's `main`{.bruijn} function has an additional abstraction
that gets applied with a lazy list of input bytes. These bytes are
encoded as the syntactic sugar encoding of binary numbers, which can be
manipulated with [`std/Number/Binary`](/std/Number_Binary.bruijn.html).

You can use [`std/Monad`](/std/Monad.bruijn.html) to interact with the
input monadically, or simply use [`std/List`](/std/List.bruijn.html)
operations to work with the input as a normal list.

## Example

``` bruijn
:import std/List .

# reverse the input list
main [<~>0]
```

``` bash
$ printf "tacocat" | bruijn reverse.bruijn
tacocat
```

[^1]: [Tromp, John. "Functional Bits: Lambda Calculus based Algorithmic
    Information Theory." (2023).](https://tromp.github.io/cl/LC.pdf)

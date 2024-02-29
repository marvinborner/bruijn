# Reduction

Bruijn supports several reducers that can be chosen using its
`--reducer` flag.

## HigherOrder [`(source)`](https://github.com/marvinborner/bruijn/blob/main/src/Reducer/HigherOrder.hs)

HigherOrder reduction is one of the simplest reducers. By translating
the entire expression to a higher-order representation, we can abuse
Haskell's internal reduction implementation in our favour. Aside from
conversion from/to the higher-order encoding, this reducer does
basically nothing special.

## RKNL [`(source)`](https://github.com/marvinborner/bruijn/blob/main/src/Reducer/RKNL.hs)

RKNL[^1] is an abstract machine for reducing lambda calculus. It uses
the call-by-need reduction strategy, similar to Haskell and other
functional programming languages. For you this means that you have
efficient support for [laziness](../coding/laziness.md) with generally
less redundant reductions.

## ION [`(source)`](https://github.com/marvinborner/bruijn/blob/main/src/Reducer/ION.hs)

[The ION machine](https://crypto.stanford.edu/~blynn/compiler/ION.html)
was created by Benn Lynn as a reducer for a hypothetical functional
stack machine computer. We convert the lambda calculus term to
combinatory logic using [Kiselyov
translation](https://crypto.stanford.edu/~blynn/lambda/kiselyov.html),
set up a "virtual" machine with the combinators, and let it run until
the stack has reached its end.

Most of the work was done by John Tromp in his
[nf.c](https://github.com/tromp/AIT/commits/master/nf.c). The
translation to Haskell and its integration into bruijn was mainly done
as an experiment on performance.

[^1]: [Biernacka, Małgorzata, Witold Charatonik, and Tomasz Drab. "A
    simple and efficient implementation of strong call by need by an
    abstract machine." Proceedings of the ACM on Programming Languages
    6.ICFP (2022): 109-136.](https://doi.org/10.5281/zenodo.6786796)

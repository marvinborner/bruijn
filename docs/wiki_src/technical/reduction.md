# Reduction

Bruijn uses the RKNL abstract machine reducer[^1]. RKNL uses the
call-by-need reduction strategy, similar to Haskell and other functional
programming languages. For you this means that you have efficient
support for [laziness](../coding/laziness.md) with generally less
redundant reductions.

[^1]: [Biernacka, Małgorzata, Witold Charatonik, and Tomasz Drab. "A
    simple and efficient implementation of strong call by need by an
    abstract machine." Proceedings of the ACM on Programming Languages
    6.ICFP (2022): 109-136.](https://doi.org/10.5281/zenodo.6786796)

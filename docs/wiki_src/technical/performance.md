# Performance

In general, the reduction of practical programs encoded in lambda
calculus is not very efficient when compared to traditional programming
languages. We do, however, work a lot on making the performance as
comparable as possible:

-   We have different reducers and constantly benchmark and improve them
    in order to find the most efficient method of reduction. Read more
    about our [reducer choices](reduction.md).
-   Bruijn uses efficient data structures by default. For example, for
    nary numbers we use results of Torben Mogensens investigations (as
    described in [number/byte encodings](../coding/data-structures.md)).
-   The lambda calculus optimizers
    [BLoC](https://github.com/marvinborner/bloc) and
    [BLoCade](https://github.com/marvinborner/blocade) are directly
    integrated into bruijn and can be enabled optionally (see
    [compilation](../coding/compilation.md))

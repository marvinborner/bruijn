# Performance

The reduction of lambda calculus is (practically) not very efficient. As
an extension, bruijn also suffers from bad performance.

Bruijn's interpreter works by substituting the entire program into one
huge lambda calculus term that will then get reduced by the
[reducer](reduction.md). As a result, many equivalent terms get
evaluated multiple times (although some of this is solved by bruijn's
call-by-need reduction strategy). We currently work on a solution that
reduces all equivalent terms as one, which turns out is not actually
that trivial. Follow the [blog](https://text.marvinborner.de) to keep up
to date with the development.

Aside from that, bruijn is still much faster than most of the hobby
programming languages based on pure lambda calculus. This is because of
the [RKNL reducer](reduction.md) and our choice of default [number/byte
encodings](../coding/data-structures.md).

``` bruijn
> :import std/Math .
> :time fac (+30)
0.15 seconds
```

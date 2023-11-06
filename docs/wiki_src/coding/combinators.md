# Combinators

Combinators are short *closed* terms that can be combined with other
terms or combinators.

## Common

All of these combinators (and many more) can be found in
[`std/Combinator`](/std/Combinator.bruijn.html). The names are taken
from Raymond Smullyan's book "To Mock a Mockingbird"[^1].

`y`{.bruijn}/`z`{.bruijn}: *Fixed-point* combinators

:   used to achieve [recursion](recursion.md)

:   `(y g)`{.bruijn} = `(g (y g))`{.bruijn}

`b`{.bruijn}/`b'`{.bruijn}/`b'''`{.bruijn} or `…∘…`{.bruijn}/`…∘∘…`{.bruijn}/`…∘∘∘…`{.bruijn}: *Blackbird* combinators

:   used to compose two functions with 1/2/3 arguments

:   `((f ∘ g) x)`{.bruijn} = `(f (g x))`{.bruijn}

:   `(((f ∘∘ g) x) y)`{.bruijn} = `(f ((g x) y))`{.bruijn}

:   `((((f ∘∘∘ g) x) y) z)`{.bruijn} = `(f (((g x) y) z))`{.bruijn}

`c`{.bruijn} or `\‣`{.bruijn}: *Cardinal* combinator

:   used to flip arguments (e.g. for higher-order application)

:   `((\f x) y)`{.bruijn} = `((f y) x)`{.bruijn}

`s`{.bruijn} or `…<*>…`{.bruijn}: *Starling* combinator

:   used to apply one argument to two functions (*substitution*)

:   `((f <*> g) x)`{.bruijn} = `((f x) (g x))`{.bruijn}

`k`{.bruijn} or `const`{.bruijn}: *Kestrel* combinator

:   used to wrap a term inside an additional abstraction (also for
    [boolean logic](data-structures.md#booleansbits-stdlogic))

:   `(k f)`{.bruijn} = `[f]`{.bruijn}

`i`{.bruijn} (Haskell's `id`{.haskell}): *Kestrel* combinator

:   used as identity function or to indicate an unused argument

:   `(i x)`{.bruijn} = `x`{.bruijn}

`ψ`{.bruijn}: *Psi* combinator (Haskell's `on`{.haskell})

:   used to apply two arguments to one function seperately

:   `((((ψ f) g) x) y)`{.bruijn} = `((f (g x)) (g y))`{.bruijn}

`ω`{.bruijn}: *Mockingbird*/*omega* combinator

:   used to apply a term to itself

:   `(ω f)`{.bruijn} = `(f f)`{.bruijn}

:   Also: `Ω`{.bruijn} = `(ω ω)`{.bruijn}

------------------------------------------------------------------------

If you enjoy the use of combinators, you might also enjoy bruijn's
sister language [Birb](https://esolangs.org/wiki/Birb).

[^1]: Smullyan, Raymond M. To Mock a Mockingbird: and other logic
    puzzles including an amazing adventure in combinatory logic. Oxford
    University Press, USA, 2000.

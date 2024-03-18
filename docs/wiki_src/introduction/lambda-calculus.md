# Lambda calculus

Bruijn is based on de Bruijn indexed lambda calculus.

## Traditional lambda calculus

Lambda calculus basically has three types of terms:

-   *Variable*: `x` binds the named variable `x` to an abstraction.
-   *Abstraction*: `λx.E` accepts an argument `x` and binds it to term
    `E` respectively. It's helpful to think of abstractions as anonymous
    functions.
-   *Application*: `(f x)` applies `f` to `x` -- the standard convention
    allows repeated left-associative application: `f x y z` is
    `(((f x) y) z)`.

Combining these terms and removing redundant parentheses can result in
terms like `λx.λy.x y`, basically representing a function with two
parameters that uses its second parameter as an argument for its first.

Evaluating terms is called *reduction*. There's only one rule you need
to know: `(λx.E A)` becomes `E[x := A]` -- that is, calling an
abstraction with an argument substitutes the argument inside the body of
the abstraction ("β-reduction"). There are many different kinds of
reduction techniques, but they basically all come back to this simple
rule -- mainly because of the "Church-Rosser theorem" that states that
the order of reduction doesn't change the eventual result.

When we talk about reduction in bruijn, we typically mean "reduction
until normal form" -- we reduce until the term can't be reduced any
further (there does not exist any `(λx.E A)`).

    (λx.x λx.x)     ⤳ λx.x
    (λx.x λx.λy.x)  ⤳ λx.λy.x
    (λx.λy.x (λx.x) ⤳ λy.λx.x

## De Bruijn indices

Programs written in lambda calculus often have many abstractions and
therefore at least as many variables. I hate naming variables,
especially if you need hundreds of them for small programs. With that
many variables it's also really complicated to compare two expressions,
since you first need to resolve shadowed and conflicting variables
("α-conversion").

De Bruijn indices replace the concept of variables by using numeric
references to the abstraction layer. Let me explain using an example:
The expression `λx.x` becomes `λ0` -- the `0` refers to the first
parameter of the abstraction. Subsequently, the expression `λx.λy.x y`
becomes `λλ1 0`. Basically, if you're reading from left to right
starting at the abstraction you want to bind, you increment on every
occurring `λ` until you arrive at the index.

While confusing at first, programs written with de Bruijn indices can
actually be way easier to understand than the equivalent program with
named variables.

## Bruijn's bracketing

Bruijn's final syntactic variation from lambda calculus is the use of
square brackets instead of lambda symbols. By putting the entire
abstracted term in brackets, it's much clearer where indices and the
respective terms are bound to.

Representing `λλ1 0` in bruijn's syntax then becomes `[[1 0]]`{.bruijn}.
The application of `λ0` and `λλ1 0` becomes `[0] [[1 0]]`{.bruijn}.

------------------------------------------------------------------------

Random example reductions:

``` bruijn
a [0] [[1]]       ⤳ [[1]]
b [[0]] [[1]]     ⤳ [0]
c [[1]] [[1]]     ⤳ [[[1]]]
d [[0]] [0] [[1]] ⤳ [[1]]
e [[0 1]] [0]     ⤳ [0 [0]]
f [[1 0]] [0]     ⤳ [0]
```

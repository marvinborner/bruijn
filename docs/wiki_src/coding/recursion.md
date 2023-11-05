# Recursion

Just as normal lambda calculus, bruijn does *not* support typical
recursion.

If you want to recursively call a function (or imitate `for`/`while`
loops), you need to use *fixed-point combinators* like `y`{.bruijn} from
[`std/Combinator`](/std/Combinator.bruijn.html).

Fixed-point combinators have the fascinating property of inducing
recursive behaviour in programming languages without support for
recursion.

Say we want a function `g`{.bruijn} to be able to call itself. With the
`y`{.bruijn} combinator the following equivalence is obtained:

``` bruijn
  (y g)
⤳ [[1 (0 0)] [1 (0 0)]] g
⤳ [g (0 0)] [g (0 0)]
⤳ g ([g (0 0)] [g (0 0)])
≡ g (y g)
```

With this equivalence, `g`{.bruijn} is able to call itself since its
outer argument is the initial function again.

Example for using `y`{.bruijn} to find the factorial of 2:

``` bruijn
# here, `1` is the outer argument (y g)
# `0` is the accumulator (the argument of `factorial`)
g [[=?0 (+1) (0 ⋅ (1 --0))]]

factorial y g ⧗ Number → Number

:test ((factorial (+3)) =? (+6)) (true)
```

In-the-wild, this could look like this.

``` bruijn
# 3 abstractions => two arguments
# 2 is recursive call
# 1 is accumulator (+0)
# 0 is argument (list)
length z [[[rec]]] (+0) ⧗ (List a) → Number
    rec 0 [[[case-inc]]] case-end
        case-inc 5 ++4 1
        case-end 1
```

Also see [coding style](style.md) for other style suggestions.

## Mutual recurrence relations

For solving mutual recurrence relations, you can use the *variadic
fixed-point combinator* `y*`{.bruijn} from
[`std/List`](/std/List.bruijn.html). This combinator produces all the
fixed points of a function as an iterable [list](data-structures.md).

Example `even?`{.bruijn}/`odd?`{.bruijn} implementation using
`y*`{.bruijn}:

``` bruijn
# the odd? recursive call will be the second argument (1)
g [[[=?0 true (1 --0)]]]

# the even? recursive call will be the first argument (2)
h [[[=?0 false (2 --0)]]]

even? head (y* g h) ⧗ Number → Bool

odd? tail (y* g h) ⧗ Number → Bool
```

Read more about this in the blog post [Variadic fixed-point
combinators](https://text.marvinborner.de/2023-06-18-15.html).

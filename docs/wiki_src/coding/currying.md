# Currying

Lambda calculus naturally supports currying -- that is, only *partially*
applying a function. In fact *any* function can be applied with *any*
amount of arguments!

In bruijn, currying is a great way to make functions even more elegant.

For example, take the negation function:

``` bruijn
# subtracts argument from zero
-‣ [(+0) - 0] ⧗ Number → Number

# equivalent curried version
-‣ sub (+0)
```

Currying is also very useful for higher-order functions.

Multiplying values in a list by partially applying the `mul`{.bruijn}
function:

``` bruijn
# doubles numbers in a list
double-list [0 <$> (mul (+2))] ⧗ (List Number) → (List Number)

:test (double-list ((+1) : {}(+2))) ((+2) : {}(+4))
```

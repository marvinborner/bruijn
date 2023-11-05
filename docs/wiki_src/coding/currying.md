# Currying

Lambda calculus naturally supports currying -- that is, only partially
applying a function. In fact *any* function can be applied with *any*
amount of arguments!

In bruijn, currying is a great way to make functions even more elegant.

Partially applying the `mul`{.bruijn} function:

``` bruijn
six [0 (+3)] (mul (+2))
```

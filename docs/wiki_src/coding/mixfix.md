# Mixfix

Mixfix functions allow arbitrary infix operations based on "substitution
holes" by using the `…` symbol in (special character) definitions. The
symbols and terms always need to be delimited by a space character, else
they get interpreted as a [prefix](prefix.md).

Example:

``` bruijn
…+… add

# the "holes" get applied in normal order
:test ((+4) + (+3)) (add (+4) (+3))
```

You can define as many holes as you like. Make sure to place parenthesis
for applications inside substitution holes.

``` bruijn
{…<$>…|… [[[2 - 1 + 0]]]

# evaluated as (5 - 2) + 1 = 4
:test ({ (+5) <$> (+2) | (+1)) ((+4))
:test ({ ((+3) + (+1)) <$> (+2) | (+1)) ((+4))
```

You can use them as normal functions by writing the identifier
literally:

``` bruijn
:test (…+… (+4) (+3)) (add (+4) (+3))
```

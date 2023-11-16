# Mixfix

Mixfix functions allow arbitrary infix operations based on "substitution
holes" by using the `…` symbol in definitions. The symbols and terms
always need to be delimited by a space character, otherwise they get
interpreted as a [prefix](prefix.md).

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
:test ({ ((+3) + (+2)) <$> (+2) | (+1)) ((+4))
```

You can use them as normal functions by writing the identifier
literally:

``` bruijn
:test (…+… (+4) (+3)) (add (+4) (+3))
```

## Associativity

If you write several mixfix operations without parenthesis, they will be
reduced in left-associative order. Just make sure that the longer mixfix
chain is not actually overwritten by *another* mixfix chain.

``` bruijn
:test ((+8) + (-4) ⋅ (-2)) ((-8))

# (don't do this)
…+…⋅… [[[(+16)]]]

:test ((+8) + (-4) ⋅ (-2)) ((+16))
```

## Allowed characters

Mixfix functions can use any characters of `!?*@:;+-_#$%^&<>/\|{}~=` as
well as mathematical unicode operators and arrows. Each part must be at
least 1 character long.

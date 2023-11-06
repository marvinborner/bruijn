# Prefix

Prefix functions are symbols written directly in front of another term
(without space). The term gets applied as an argument to the prefix
function. Use [mixfix functions](mixfix.md) if the function has more
than one argument.

They are defined by the `‣`{.bruijn} suffix.

Example:

``` bruijn
# defines a negation prefix function called '-'
-‣ [(+0) - 0] ⧗ Number → Number

# returns 0 - 10 = -10
:test (-(+10)) ((-10))
```

You can use them as normal functions by writing the identifier
literally:

``` bruijn
:test (-‣ (+10)) ((-10))
```

## Allowed characters

Prefix functions can use any characters of `!?*@:;+-_#$%^&<>/\|{}~=` as
well as mathematical unicode operators and arrows. They must be at least
1 character long.

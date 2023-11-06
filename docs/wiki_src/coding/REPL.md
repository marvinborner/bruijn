# REPL

The REPL is a very helpful tool for functional programming languages
like bruijn. You can use it to continuously test or execute parts of
your code.

You can start the REPL using `stack run`{.bash} or (if installed)
`bruijn`{.bash}.

Any valid term will get reduced to normal form after pressing enter.
Common [data structures](data-structures.md) will get resolved in a
seperate line if detected (e.g. numbers, lists or strings).

## Definitions

Since everything you type will get evaluated, definitions (compared to
definitions in files) require an equal sign:

``` bruijn
> id = [0]
> id
[0]
```

## Commands

### `:import`{.bruijn}/`:input`{.bruijn}

Equivalent to the [respective commands in
files](../introduction/syntax.md#imports).

``` bruijn
> :import std/Math .
```

### `:test`{.bruijn}

Equivalent to the [test command in
files](../introduction/syntax.md#tests).

``` bruijn
> :test ([0]) ([[1]])
ERROR test failed: [0] = [[1]]
      reduced to [0] = [[1]]
```

### `:watch`{.bruijn}

`:watch`{.bruijn} re-imports the file automatically after every saved
change. It will rerun any test the watched file contains.

``` bruijn
> :watch collatz-proof
```

This can be very helpful for [test driven
development](test-driven-development.md).

### `:time`{.bruijn}

Measures the time from start of reduction to its end (normal form) in
seconds.

``` bruijn
> :time fac (+30)
0.15 seconds
```

### `:blc`{.bruijn}

Translates both the unreduced and the reduced expression to binary
lambda calculus. Helpful for golfed [compilation](compilation.md).

``` bruijn
> :blc [0] [0]
0100100010
0010
```

### `:length`{.bruijn}

Measures the length of the binary lambda calculus encoding of both the
unreduced and the reduced expression. Helpful for golfed
[compilation](compilation.md).

``` bruijn
> :length [0] [0]
10
4
```

### `:free`{.bruijn}

The `free` command frees the current environment including all defined
identifiers and imported files.

``` bruijn
> id = [0]
> :free
> id
ERROR undefined identifier id
```

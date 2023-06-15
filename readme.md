<p align="center">
<img width="200" src="https://raw.githubusercontent.com/marvinborner/bruijn/main/docs/res/logo.png" alt="Bruijn logo"/>
</p>
<h1 align="center">
bruijn
</h1>

> A purely academic programming language based on lambda calculus and De
> Bruijn indices written in Haskell.

[Jump to examples](#Examples) or use the navigation tree to jump to
other sections.

Docs, articles, examples and more:
[website](https://bruijn.marvinborner.de).

## Features

- **De Bruijn indices[\[0\]](#References)** eliminate the complexity of
  α-equivalence and α-conversion
- Unique **bracket-style representation** for lambda abstractions
  enables improved human-readability and faster syntactic perception
- **Call-by-need** reduction with great time/memory complexity by using
  the RKNL[\[4\]](#References) abstract machine (similar to
  [calm](https://github.com/marvinborner/calm/))
- **Syntactic sugar** for unary/binary/ternary numerals and
  binary-encoded strings and chars
- **No primitive functions** - every function is implemented in Bruijn
  itself
- Highly space-efficient compilation to **binary lambda calculus
  (BLC)[\[2\]](#References)[\[3\]](#References)** additionally to normal
  interpretation and REPL
- Strongly **opinionated parser** with strict syntax rules
- **Recursion** can be implemented using combinators such as Y, Z or ω
- Substantial **standard library** featuring many useful functions (see
  `std/`)

## Basics

### De Bruijn indices

De Bruijn indices[\[0\]](#References) replace the concept of variables
in lambda calculus. The index basically represents the abstraction layer
you want to reference beginning at 0 with the innermost layer.

For example, λx.x becomes λ0 because x referenced the first abstraction
layer. Furthermore, λx.λy.xy becomes λλ10 and so forth.

You can read more about De Bruijn indices on
[Wikipedia](https://en.wikipedia.org/wiki/De_Bruijn_index).

### Syntax

In general the syntax of bruijn is pretty similar to the previously
presented normal lambda calculus syntax with De Bruijn indices. The main
difference of the syntax of expressions is the usage of square brackets
instead of λs (e.g. `[[1 0]]` instead of λλ10).

You can use any function that you’ve previously defined. You can also
overwrite previously defined functions. The environment gets interpreted
from bottom to top (starting at `main`).

The following are the main syntax specifications in the (minorly
extended) [Backus-Naur
form](https://en.wikipedia.org/wiki/Backus%E2%80%93Naur_form)
(prefix/infix/mixfix operators are omitted for simplicity).

    <identifier>  ::= [a-ω,A-Ω,_][a-ω,A-Ω,0-9,?,!,',-]*
    <namespace>   ::= [A-Ω][a-ω,A-Ω]+
    <abstraction> ::= "[" <expression> "]"
    <number>      ::= ("+" | "-")[0-9]+[u|b|t]
    <bruijn>      ::= [0-9]
    <singleton>   ::= <bruijn> | <number> | <abstraction> | "(" <application> ")" | [namespace.]<identifier>
    <application> ::= <singleton> <singleton>
    <expression>  ::= <application> | <singleton>
    <test>        ::= ":test " "(" <expression> ") (" <expression> ")"
    <import>      ::= ":import " <path> [namespace]
    <input>       ::= ":input " <path>
    <comment>     ::= "# " <letter>*

Differences in syntax between REPL and file:

**For files**:

The execution of a file begins at the `main` function. Its existence is
mandatory. Note the missing equal sign in definitions.

    <definition> ::= <identifier> <expression>
    <line>       ::= <definition> | <comment> | <import> | <test> | "\n"

**For REPL**:

    <definition> ::= <identifier> = <expression>
    <watch>      ::= ":watch " <path>
    <time>       ::= ":time " <expression>
    <free>       ::= ":free"
    <line>       ::= <definition> | <expression> | <comment> | <import> | <watch> | <test> | <time> | <free> | "\n"

### Numbers

Numbers in bruijn always have a sign in front of them or else they will
be mistaken for De Bruijn indices. They also need to be between
parenthesis because of prefix functions. You can then enter a character
indicating the desired base of the number.

Generally the decimal representation is only syntactic sugar for a
lambda calculus encoding. The default base is balanced ternary because
it’s a great compromise between performance and size (according to
[\[1\]](#References)). The currently supported base suffixes are ‘t’ for
balanced ternary (default), ‘b’ for binary and ‘u’ for unary
(church-encoded).

You don’t have to care about the internals too much though as long as
you use the included operations from the standard library
(`std/Number/`). The REPL even tries its best at displaying decimal
numbers directly for expressions that look like numbers.

For example, in the REPL:

    λ (+42)
    *> [[[[(0 (2 (2 (2 (1 3)))))]]]]
    ?> 42t

    λ (+42b)
    *> [[[(0 (1 (0 (1 (0 (1 2))))))]]]
    ?> 42b

### Standard library

You may want to use the included standard library to reach your
program’s full potential. It includes many common combinators as well as
functions for numerical, boolean and IO operations and much more.

For example, you can import the standard library for numbers using
`:import std/Number`. You can find all available libraries in the `std/`
directory.

### Broogle

You can use the broogle script (inspired by Haskell’s hoogle) to search
for standard library functions by type, name or comment:

    ./broogle.sh -t "a -> a"
    ./broogle.sh -f "i"
    ./broogle.sh -c "idiot combinator"

The script uses the dependencies `rg`, `jq`, `sed` and `awk`.

### Examples

You can find more “real world” examples here: [samples](/samples).

You can try these by experimenting in the REPL or by running them as a
file.

**Remember** that you need an equal sign between the function name and
its definition if you’re using the REPL.

#### Plain execution without any predefined functions

Without using its standard library bruijn is basically unmodified pure
lambda calculus with syntactically sugared numerals, strings and chars.
Bruijn doesn’t support any numerical operations or any other
infix/prefix functions by default. Using it without its standard library
can be quite fun, though - especially for exploring and understanding
the logic of lambda calculus:

    # this is a comment
    # we now define a function returning a ternary 1 (syntactic sugar)
    get-one (+1)

    # we can use the function in all functions below its definition
    get-one2 get-one

    # tests are similar to assertions in other languages
    # they test equality using α-equivalence of reduced expressions
    # in this example they're used to show the reduced expressions
    :test (get-one2) ((+1))

    # remember that numbers always need to be written in parenthesis
    # therefore two braces are needed in tests because testing exprs
    # must always be in parenthesis as well

    # indenting acts similarly to Haskell's where statement
    get-one3 foo
        bar (+1)
        foo bar

    # equivalent of λx.x or Haskell's id x = x
    id [0]

    # testing equivalent of (λx.x) (λx.λy.x) = λx.λy.x
    # the numbers in the abstractions refer to arguments using
    # De Bruijn indices
    :test (id [[1]]) ([[1]])

    # prefix function definition
    !‣ [[1]]

    # use prefix function '!'
    # ![0] becomes ([[1]] [0]) which in turn becomes [[0]]
    :test (![0]) ([[0]])

    # infix function definition: flip and apply arguments
    …<>… [[0 1]]

    # use infix function '<>'
    # [[0]] <> [[1]] becomes (([[0 1]] [[0]]) [[1]])
    :test ([[0]] <> [[1]]) ([[1]] [[0]])

    # multiple arguments
    number-set set-of-three (+1) (+2) (+3)
        set-of-three [[[[0 1 2 3]]]]

    access-first [0 [[[0]]]]

    :test (access-first number-set) ((+1))

    # ignore args and return string 
    main ["Hello world!\n"]

#### Using standard library

Concatenating “Hello world” program using IO (the first argument of the
`main` function is a binary representation of stdin):

    :import std/List .

    main ["Hello " ++ 0 ++ "!\n"]

You can then use `printf "world" | bruijn file.bruijn` to get “Hello
world!”

Some other great functions:

    :import std/Combinator .
    :import std/List .
    :import std/Logic .
    :import std/Number .
    :import std/Option .
    :import std/Pair .

    # pairs with some values
    love pair me you
        me [[[1]]]
        you [[[2]]]

    :test (fst love) ([[[1]]])
    :test (snd love) ([[[2]]])

    # you can also write (me : you) instead of (pair me you)
    # also (^love) and (~love) instead of (fst love) and (snd love)

    # numerical operations
    # remember that every mixfix chain is left-associative
    five --((+8) + (-4) - (-2))

    not-five? [if (0 =? (+5)) false true]

    # awesome mixfix functions
    :test (∑ (+1) → (+3) | [++0]) ((+9))
    :test (∏ (+1) → (+3) | [++0]) ((+24))

    :test (not-five? five) (false)

    :test ((uncurry mul (pair (+3) (+2))) =? (+6)) (true)

    # lazy evaluation using infinite lists and indexing
    pow2 …!!… (iterate (…⋅… (+2)) (+1))

    :test ((pow2 (+5)) =? (+32)) (true)

    # options
    :test (map inc (some (+1))) (some (+2))
    :test (apply (some (+1)) [some ++0]) (some (+2))

    # boolean
    # the main function gets executed automatically
    # ignore stdin arguments by not referencing 0
    main [¬(false ⋀? true ⋁? true)]

    :test (main [0]) (false)

Read the files in `std/` for an overview of all functions/libraries or
visit the interactive [website](https://bruijn.marvinborner.de).

### Compilation

You can compile bruijn to John Tromp’s
BLC[\[2\]](#References)[\[3\]](#References). Only the used functions
actually get compiled in order to achieve a minimal binary size.

BLC uses the following encoding:

| term         | lambda | bruijn | BLC              |
|:-------------|:-------|:-------|:-----------------|
| abstraction  | λM     | \[M\]  | 00M              |
| application  | MN     | MN     | 01MN             |
| bruijn index | i      | i      | 1<sup>i+1</sup>0 |

You may also want to use my [BLoC](https://github.com/marvinborner/bloc)
format, which removes redundant duplicates – very useful to reduce the
size of typical bruijn programs. Typical workflow using the `bloc` util:

    bruijn -B program.bruijn | bloc --from-blc -i - -o out.bloc
    cat input | bruijn -E <(bloc --from-bloc -i out.bloc)

## Installation

You first need to install Haskell and Haskell Stack using the guidelines
of your operating system.

Using Haskell Stack, run `stack run` or `stack run -- [args]` to play
around and use `stack install` to install bruijn into your path.

## REPL config

You can configure the REPL by editing the `config` file. `stack install`
or `stack run` will move the file into a data directory.

More options can be found
[here](https://github.com/judah/haskeline/wiki/UserPreferences).

## Usage

Please read the usage information in the executable by using the `-h`
argument.

## References

0.  De Bruijn, Nicolaas Govert. “Lambda calculus notation with nameless
    dummies, a tool for automatic formula manipulation, with application
    to the Church-Rosser theorem.” Indagationes Mathematicae
    (Proceedings). Vol. 75. No. 5. North-Holland, 1972.
1.  Mogensen, Torben. “An investigation of compact and efficient number
    representations in the pure lambda calculus.” International Andrei
    Ershov Memorial Conference on Perspectives of System Informatics.
    Springer, Berlin, Heidelberg, 2001.
2.  Tromp, John. “Binary lambda calculus and combinatory logic.”
    Randomness and Complexity, from Leibniz to Chaitin. 2007. 237-260.
3.  Tromp, John. “Functional Bits: Lambda Calculus based Algorithmic
    Information Theory.” (2022).
4.  Biernacka, M., Charatonik, W., & Drab, T. (2022). A simple and
    efficient implementation of strong call by need by an abstract
    machine. Proceedings of the ACM on Programming Languages, 6(ICFP),
    109-136.

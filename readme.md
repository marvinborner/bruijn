<p align="center">
<img width="200" src="https://raw.githubusercontent.com/marvinborner/bruijn/main/docs/res/logo.png" alt="Bruijn logo"/>
</p>

> A purely functional programming language based on lambda calculus and
> De Bruijn indices written in Haskell.

Wiki, docs, articles, examples and more:
[website](https://bruijn.marvinborner.de).

## Features

- **De Bruijn indices** eliminate the complexity of α-equivalence and
  α-conversion
- Unique **bracket-style representation** for lambda abstractions
  enables improved human-readability and faster syntactic perception
- **Call-by-need** reduction with great time/memory complexity by using
  the RKNL abstract machine (similar to
  [calm](https://github.com/marvinborner/calm/))
- **Syntactic sugar** for unary/binary/ternary numerals and
  binary-encoded strings and chars
- **No primitive functions** - every function is implemented in Bruijn
  itself
- Highly space-efficient compilation to **binary lambda calculus (BLC)**
  additionally to normal interpretation and REPL
- Strongly **opinionated parser** with strict syntax rules
- **Recursion** can be implemented using combinators such as Y, Z or ω
- Substantial **standard library** featuring many useful functions (see
  `std/`)

## Wiki

Learn anything about bruijn in the
[wiki](https://bruijn.marvinborner.de/wiki/) (also found in
`docs/wiki_src/`).

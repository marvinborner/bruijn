<p align="center">

<img width="200" src="https://raw.githubusercontent.com/marvinborner/bruijn/main/docs/res/logo.png" alt="Bruijn logo"/>

</p>

> A purely functional programming language based on lambda calculus and
> de Bruijn indices written in Haskell.

Pronunciation: `/bɹaʊn/`.

Wiki, docs, articles, examples and more:
[website](https://bruijn.marvinborner.de). Also: [Rosetta
Code](https://rosettacode.org/wiki/Category:Bruijn).

## Features

- Substantial **standard library** with 700+ useful functions (see
  `std/`)
- **No primitive functions** - every function is implemented in Bruijn
  itself
- 1:1 correspondence to lambda calculus (e.g. space-efficient
  compilation to **binary lambda calculus (BLC)**)
- **de Bruijn indices** instead of named variables
- Lazy evaluation by default (**call-by-need** reduction)
- **Syntactic sugar** makes writing terms simpler (e.g. numbers,
  strings, chars, meta terms)
- **Mixfix** and **prefix** operators
- **Recursion** can be implemented using combinators such as Y, Z or ω

## Why

- By having a very small core (the reducer), bruijn is safe, consistent,
  and (potentially) proven to be correct!
- Since it doesn’t have builtin functions, bruijn is independent of
  hardware internals and could easily be run on almost any architecture.
- Compiled binary lambda calculus is incredibly expressive and tiny.
  Read the articles by [Jart](https://justine.lol/lambda/#why) and
  [Tromp](https://tromp.github.io/cl/cl.html).
- Exploring different encodings of data as function abstractions is
  really fascinating.
- Naming parameters of functions is annoying. De Bruijn indices are a
  universal reference independent of the function and can actually help
  readability!
- Really, just for fun.

## Wiki

Learn anything about bruijn in the
[wiki](https://bruijn.marvinborner.de/wiki/) (also found in
`docs/wiki_src/`).

## References

0.  De Bruijn, Nicolaas Govert. “Lambda calculus notation with nameless
    dummies, a tool for automatic formula manipulation, with application
    to the Church-Rosser theorem.” Indagationes Mathematicae
    (Proceedings). Vol. 75. No. 5. North-Holland, 1972.
1.  Mogensen, Torben. “An investigation of compact and efficient number
    representations in the pure lambda calculus.” International Andrei
    Ershov Memorial Conference on Perspectives of System Informatics.
    Springer, Berlin, Heidelberg, 2001.
2.  Wadsworth, Christopher. “Some unusual λ-calculus numeral systems.”
    (1980): 215-230.
3.  Tromp, John. “Binary lambda calculus and combinatory logic.”
    Randomness and Complexity, from Leibniz to Chaitin. 2007. 237-260.
4.  Tromp, John. “Functional Bits: Lambda Calculus based Algorithmic
    Information Theory.” (2022).
5.  Biernacka, M., Charatonik, W., & Drab, T. (2022). A simple and
    efficient implementation of strong call by need by an abstract
    machine. Proceedings of the ACM on Programming Languages, 6(ICFP),
    109-136.

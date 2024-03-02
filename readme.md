<p align="center">
<img width="200" src="https://raw.githubusercontent.com/marvinborner/bruijn/main/docs/res/logo.png" alt="Bruijn logo"/>
</p>

> A purely functional programming language based on lambda calculus and
> De Bruijn indices written in Haskell.

Pronunciation: `/bɹaʊn/`.

Wiki, docs, articles, examples and more:
[website](https://bruijn.marvinborner.de). Also: [Rosetta
Code](https://rosettacode.org/wiki/Category:Bruijn).

## Features

- **De Bruijn indices** eliminate the complexity of α-equivalence and
  α-conversion
- **Call-by-need** reduction with great time/memory complexity by using
  the RKNL abstract machine (similar to
  [calm](https://github.com/marvinborner/calm/))
- **Syntactic sugar** for unary/binary/ternary numerals and
  binary-encoded strings and chars
- **No primitive functions** - every function is implemented in Bruijn
  itself
- Highly space-efficient compilation to **binary lambda calculus (BLC)**
  additionally to normal interpretation and REPL
- **Recursion** can be implemented using combinators such as Y, Z or ω
- Substantial **standard library** featuring many useful functions (see
  `std/`)

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

# Compilation

Bruijn can be compiled to John Tromp’s binary lambda calculus (BLC).

BLC uses the following encoding:

| term         | lambda | bruijn  | BLC              |
|:-------------|:-------|:--------|:-----------------|
| abstraction  | λM     | `[M]`   | 00M              |
| application  | (MN)   | `(M N)` | 01MN             |
| bruijn index | i      | `i`     | 1<sup>i+1</sup>0 |

There are two modes of compilation:

- **Bitwise** compiles to BLC and encodes every bit as 1 bit and pads
  the last remaining byte: `bruijn -b path`
- **ASCII** compiles to BLC and encodes every bit as 1 ASCII character
  (`'0'`/`'1'`): `bruijn -B path`

## Compilation overhead

Typical compilation to BLC results in much redundant code, since every
used function gets substituted and translated separately. In
`((+3) + (+4) + (+3))`, for example, `add` gets compiled to BLC two
times, resulting in a redundant overhead of around 3500 bits.

This is because BLC was never intended for compilation of normal
programs, but mainly as an academic encoding model. This also means that
it’s quite good for writing very expressive and minimal programs
(i.e. obfuscated code golfing, see [John Tromp’s
IOCCC](https://ioccc.org/2012/tromp/hint.html)).

Most programs, however, won’t be golfed and can result in rather large
compiled programs. While there’s not really any practical need for
compilation aside from golfing, you could still use the
[BLoC](https://github.com/marvinborner/bloc) project to optimize
redundant terms.

Typical workflow:

``` bash
$ bruijn -B program.bruijn | bloc --from-blc -i - -o out.bloc
$ cat input | bruijn -E <(bloc --from-bloc -i out.bloc)
```

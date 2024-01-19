# Compilation

Bruijn can be compiled to John Tromp's binary lambda calculus (BLC).

BLC uses the following encoding:

| term         | lambda | bruijn  | BLC              |
|:-------------|:-------|:--------|:-----------------|
| abstraction  | λM     | `[M]`   | 00M              |
| application  | (MN)   | `(M N)` | 01MN             |
| bruijn index | i      | `i`     | 1<sup>i+1</sup>0 |

There are two modes of compilation:

-   **Bitwise** compiles to BLC and encodes every bit as 1 bit and pads
    the last remaining byte: `bruijn -b path`
-   **ASCII** compiles to BLC and encodes every bit as 1 ASCII character
    (`'0'`/`'1'`): `bruijn -B path`

## Compilation overhead

By default, bruijn's compilation to BLC results in much redundant code,
since every used function gets substituted and translated separately. In
`((+3) + (+4) + (+3))`{.bruijn}, for example, `add`{.bruijn} gets
compiled to BLC two times, resulting in a redundant overhead of around
3500 bits.

If you want smaller (and more efficient) files, install
[BLoC](https://github.com/marvinborner/BLoC) and
[BLoCade](https://github.com/marvinborner/BLoCade). The combination of
these tools results in the abstraction of shared terms and translation
to a specified target.

With the bruijn CLI, BLoCade can be executed directly using the flag
`-t TARGET`, where `TARGET` is one of the supported targets.

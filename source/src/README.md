Comments on the source code
===========================

BNFC is boot-strapped:
The `*BNF.hs` files are created from `BNF.cf` by BNFC itself.

2018-03-27

  If you want to use `<>` from pretty you must include `Prelude'` instead of
  `Prelude`. See [#227](https://github.com/BNFC/bnfc/pull/227).
  This is hopefully a temporary hack.

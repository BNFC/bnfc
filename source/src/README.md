Comments on the source code
===========================

2018-03-27

  If you want to use `<>` from pretty you must include `Prelude'` instead of
  `Prelude`. See #227. This is hopefully a temporary hack.

2017-11-21

  The LBNF grammar is currently not boot-strapped,
  since a few of the generated files have been modified by hand,
  including:

    AbsBNF.hs
    PrintBNF.hs

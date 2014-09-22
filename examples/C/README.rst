C grammars
==========

This directory contains different grammars related to the C language:

``C4.cf``
  A small subset of C. Used to teach programming language construction and
  bnfc to students at chalmers. It should be enough to parse ``koe2.c``
``C.cf``
  `Ansi C`_, following Kernighan & Ritchie (Second Edition).
  Developed by Ulf Persson is 2003 as part of a BSc thesis at
  Chalmers University of Technology
``C_with_delimiters.cf``
  Modified Ansi C grammar to make use of ``delimiter`` pragmas (used in the CNF
  backend).

And some example programs:

``koe2.c``
  A very simple program that prints the first 12 factorials
``runtime.c``
  Runtime for the C4 language. Defines some "built-in" function (printX and
  readX). This is given to the student but serves here as an example of Ansi C
  program.
``core.c`` and ``small.c``
  C examples that have been modified a bit and can now only be parsed with the
  ``C_with_delimiters.cf`` grammar.

.. _Ansi C: https://en.wikipedia.org/wiki/ANSI_C

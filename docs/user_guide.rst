==========
User Guide
==========

.. contents::

Pygments
========

Pygments is not really a compiler front-end tool, like lex and yacc, but a
widely used syntax highlighter (used for syntax highlighting on github among
others).

With the ``--pygments`` option, BNFC generates a new python lexer to be used
with pygments.

Usage
.....

There is two ways to add a lexer to pygments:

* Fork the pygments codebase and add your lexer in ``pygments/lexers/``
* Install your lexer as a pygments plugin using setuptools

In addition to the lexer itself, BNFC will generate an minimal installation
script ``setup.py`` for the second option so you can start using the
highlighter right away without fiddling with pygments code.

Here is an example (assuming you've put the Calc grammar in the current
directony)::

    virtualenv myenv    # If you don't use virtualenv, skip the first two steps
    source myenv/bin/activate
    bnfc --pygments Calc.cf
    python setup.py install
    echo "1 + 2 - 3 * 4" | pygmentize -l calc

You should see something like:

.. image:: /calc-pygments.png

Here is the LBNF grammar highlighted with the pygments lexer generated from it:

.. image:: /lbnf-pygments.png

Caveats
.......

The generated lexer has very few highlighting categories. In particular, all
keywords are highlighted the same way, all symbols are highlighted the same way
and it doesn't use context (so, for instance, it cannot differentiate the same
identifier used as a function definition and a local variable...)

Pygments makes it possible to register file extensions associated with a lexer.
BNFC adds the grammar name as a file extension. So if the grammar file is
named ``Calc.cf``, the lexer will be associated to the file extension
``.calc``. To associate other file extensions to a generated lexer, you need to
modify (or subclass) the lexer.

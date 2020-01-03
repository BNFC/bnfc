Test suite for ill-formed input
===============================

This directory contains test cases that should fail the
well-formedness checker for LBNF before even passing on to a backend.

To add a test case, place two files here:

  * _name_`.cf`: The errorneous LBNF file.
  * _name_`.err`: The expected error output.

Each `.cf` file will be run through BNFC and the error message will be
compared to the contents of the respective `.err` file.

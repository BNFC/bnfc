#include <stdio.h>
#include <stdlib.h>

#include "Parser.h"
#include "Printer.h"
#include "Absyn.h"
#include "Interpreter.h"

int main(int argc, char ** argv)
{
  FILE *input;
  input = stdin;
  Exp parse_tree = pExp(input);
  if (parse_tree)
  {
    printf("%d\n", interpret(parse_tree));
    return 0;
  }
  return 1;
}


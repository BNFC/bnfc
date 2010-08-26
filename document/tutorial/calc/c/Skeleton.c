/*** BNFC-Generated Visitor Traversal Skeleton. ***/
/* This traverses the abstract syntax tree.
   To use, copy Skeleton.h and Skeleton.c to
   new files. */

#include "Skeleton.h"

void visitExp(Exp _p_)
{
  switch(_p_->kind)
  {
  case is_EAdd:
    /* Code for EAdd Goes Here */
    visitExp(_p_->u.eadd_.exp_1);
    visitExp(_p_->u.eadd_.exp_2);
    break;
  case is_ESub:
    /* Code for ESub Goes Here */
    visitExp(_p_->u.esub_.exp_1);
    visitExp(_p_->u.esub_.exp_2);
    break;
  case is_EMul:
    /* Code for EMul Goes Here */
    visitExp(_p_->u.emul_.exp_1);
    visitExp(_p_->u.emul_.exp_2);
    break;
  case is_EDiv:
    /* Code for EDiv Goes Here */
    visitExp(_p_->u.ediv_.exp_1);
    visitExp(_p_->u.ediv_.exp_2);
    break;
  case is_EInt:
    /* Code for EInt Goes Here */
    visitInteger(_p_->u.eint_.integer_);
    break;

  default:
    fprintf(stderr, "Error: bad kind field when printing Exp!\n");
    exit(1);
  }
}

void visitIdent(Ident i)
{
  /* Code for Ident Goes Here */
}
void visitInteger(Integer i)
{
  /* Code for Integer Goes Here */
}
void visitDouble(Double d)
{
  /* Code for Double Goes Here */
}
void visitChar(Char c)
{
  /* Code for Char Goes Here */
}
void visitString(String s)
{
  /* Code for String Goes Here */
}


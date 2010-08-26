#include "Interpreter.h"
#include <stdio.h>

int interpret(Exp _p_)
{
  switch(_p_->kind)
  {
  case is_EAdd:
    return interpret(_p_->u.eadd_.exp_1) + interpret(_p_->u.eadd_.exp_2) ;
  case is_ESub:
    return interpret(_p_->u.eadd_.exp_1) - interpret(_p_->u.eadd_.exp_2) ;
  case is_EMul:
    return interpret(_p_->u.eadd_.exp_1) * interpret(_p_->u.eadd_.exp_2) ;
  case is_EDiv:
    return interpret(_p_->u.eadd_.exp_1) / interpret(_p_->u.eadd_.exp_2) ;
  case is_EInt:
    return _p_->u.eint_.integer_ ;
  default:
    fprintf(stderr, "Error: bad kind field when printing Exp!\n");
    exit(1);
  }
}

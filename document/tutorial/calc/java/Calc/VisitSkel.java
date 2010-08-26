package Calc;
import Calc.Absyn.*;
/*** BNFC-Generated Visitor Design Pattern Skeleton. ***/
/* This implements the common visitor design pattern.
   Tests show it to be slightly less efficient than the
   instanceof method, but easier to use. 
   Replace the R and A parameters with the desired return
   and context types.*/

public class VisitSkel
{
  public class ExpVisitor<R,A> implements Exp.Visitor<R,A>
  {
    public R visit(Calc.Absyn.EAdd p, A arg)
    {
      /* Code For EAdd Goes Here */

      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(Calc.Absyn.ESub p, A arg)
    {
      /* Code For ESub Goes Here */

      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(Calc.Absyn.EMul p, A arg)
    {
      /* Code For EMul Goes Here */

      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(Calc.Absyn.EDiv p, A arg)
    {
      /* Code For EDiv Goes Here */

      p.exp_1.accept(new ExpVisitor<R,A>(), arg);
      p.exp_2.accept(new ExpVisitor<R,A>(), arg);

      return null;
    }
    public R visit(Calc.Absyn.EInt p, A arg)
    {
      /* Code For EInt Goes Here */

      //p.integer_;

      return null;
    }

  }
}
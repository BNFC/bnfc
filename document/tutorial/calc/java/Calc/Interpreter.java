package Calc;
import Calc.Absyn.*;

public class Interpreter {
  
    public static Integer interpret(Exp e) 
    {
	Exp exp = (Exp)e ;
	return interpretExp(exp, null) ;
    }

    private static Integer interpretExp(Exp e, Object o) 
    {
	return e.accept(new Calculator(), null) ;
    }

    private static class Calculator implements Exp.Visitor<Integer,Object> {


    public Integer visit(Calc.Absyn.EAdd p,Object o)
    {

      Integer a = p.exp_1.accept(this, null);
      Integer b = p.exp_2.accept(this, null);

      return a + b;
    }
    public Integer visit(Calc.Absyn.EDiv p,Object o)
    {

      Integer a = p.exp_1.accept(this, null);
      Integer b = p.exp_2.accept(this, null);

      return a / b;
    }
    public Integer visit(Calc.Absyn.ESub p,Object o)
    {

      Integer a = p.exp_1.accept(this, null);
      Integer b = p.exp_2.accept(this, null);

      return a - b;
    }
    public Integer visit(Calc.Absyn.EMul p,Object o)
    {

      Integer a = p.exp_1.accept(this, null);
      Integer b = p.exp_2.accept(this, null);

      return a * b;
    }
    
    public Integer visit(Calc.Absyn.EInt p, Object o)
    {
      return p.integer_;
    }
    }    
  }

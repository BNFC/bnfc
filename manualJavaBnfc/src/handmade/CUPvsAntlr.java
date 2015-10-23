package handmade;
import language.Absyn.Ent;
import language.Test;
import language.AntlrTest;

/**
 * Created by gapag on 10/21/15.
 */
public class CUPvsAntlr {
  public static void main(String [] arg) throws Exception {
    String filename = arg[arg.length-1];
    Test t = new Test(new String[]{filename});
    AntlrTest a = new AntlrTest(arg);
    Ent e = t.parse();
    Ent ea = a.parse();
    if(!e.equals(ea)){

    }
  }
}

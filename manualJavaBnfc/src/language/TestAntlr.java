package language;

import org.antlr.v4.gui.TestRig;

/**
 * Created by gapag on 10/19/15.
 */
public class TestAntlr {

  public static void main(String [] arg) throws Exception {
    /*TestRig tr = new TestRig(arg);
    tr.process();*/
    System.out.println(String.format("%x",(int)'ß'));
    for(int i = 0xf8; i<= 0xff; i++){
      System.out.println((char)i);
      System.out.println(String.format("not %x",(char)':'));
    }
  }
}

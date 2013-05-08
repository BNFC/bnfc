{-
    BNF Converter: Java Top File
    Copyright (C) 2004  Author:  Markus Forsberg, Peter Gammie, Michael Pellauer

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

-------------------------------------------------------------------
-- |
-- Module      :  JavaTop
-- Copyright   :  (C)opyright 2003, {markus, aarne, pellauer, peteg} at cs dot chalmers dot se
-- License     :  GPL (see COPYING for details)
--
-- Maintainer  :  {markus, aarne} at cs dot chalmers dot se
-- Stability   :  alpha
-- Portability :  Haskell98
--
-- Top-level for the Java back end.
--
-- > $Id: JavaTop.hs,v 1.10 2005/09/21 13:06:10 bringert Exp $
-------------------------------------------------------------------

module JavaTop ( makeJava ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------
import System.Directory	( createDirectory )
import System.IO.Error	( tryIOError, isAlreadyExistsError )
import System.Exit      ( exitFailure )

import BNFC.Utils
import BNFC.CF
import CFtoCup		( cf2Cup )
import CFtoJLex
import CFtoJavaAbs	( cf2JavaAbs )
import CFtoJavaPrinter
import CFtoJavaSkeleton
import CFtoVisitSkel
import BNFC.Backend.Latex
import Data.Char
import Data.List(intersperse)

-------------------------------------------------------------------
-- | Build the Java output.
-- FIXME: get everything to put the files in the right places.
-- Adapt Makefile to do the business.
-------------------------------------------------------------------
makeJava :: Bool -> String -> CF -> IO ()
makeJava = mkFiles

mkFiles :: Bool -> String -> CF -> IO ()
mkFiles make name cf =
    do -- Create the package directories if necessary.
       let packageBase = name
	   packageAbsyn = packageBase ++ "." ++ "Absyn"
	   dirBase = pkgToDir packageBase
	   dirAbsyn = pkgToDir packageAbsyn
       chkExists dirBase
       chkExists dirAbsyn
       let absynFiles = remDups $ cf2JavaAbs packageBase packageAbsyn cf
	   absynBaseNames = map fst absynFiles
	   absynFileNames = map (dirAbsyn ++) absynBaseNames
       let writeAbsyn (filename, contents) =
	       writeFileRep (dirAbsyn ++ filename ++ ".java") contents
       mapM writeAbsyn absynFiles
       writeFileRep (dirBase ++ "PrettyPrinter.java") $ cf2JavaPrinter packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "Skeleton.java") $ cf2JavaSkeleton packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "Visitable.java") $ prVisitable packageBase
       let user = fst $ unzip $ tokenPragmas cf -- FIXME better var name
       writeFileRep (dirBase ++ "Visitor.java") $ prVisitor packageBase packageAbsyn absynBaseNames user
       writeFileRep (dirBase ++ "VisitSkel.java") $ cf2VisitSkel packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "Test.java") $ javaTest packageBase packageAbsyn cf
       let (lex, env) = cf2jlex packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "Yylex") lex
       putStrLn "   (Tested with JLex 1.2.6.)"
       writeFileRep (dirBase ++ name ++ ".cup") $ cf2Cup packageBase packageAbsyn cf env
       -- FIXME: put in a doc directory?
       putStrLn $ "   (Parser created for category " ++ firstEntry cf ++ ")"
       putStrLn "   (Tested with CUP 0.10k)"
       writeFileRep (name ++ ".tex") $ cfToLatex name cf
       if make
         then writeFileRep "Makefile" $ makefile name dirBase dirAbsyn absynFileNames
	 else return ()
    where
      remDups [] = []
      remDups ((a,b):as) = case lookup a as of
			     Just {} -> remDups as
			     Nothing -> (a, b) : (remDups as)

      pkgToDir :: String -> FilePath
      pkgToDir p = [ if c == '.' then '/' else c | c <- p] ++ "/"

      chkExists :: FilePath -> IO ()
      chkExists dir =
	  do eErr <- tryIOError $ createDirectory dir
	     case eErr of
	       Left ioerr -> if isAlreadyExistsError ioerr
			       then return ()
			       else fail $ show ioerr
	       Right ()   -> putStrLn $ "Created directory: " ++ dir

-- FIXME get filenames right.
-- FIXME It's almost certainly better to just feed all the Java source
-- files to javac in one go.
-- Replace with an ANT script?
makefile :: String -> FilePath -> FilePath -> [String] -> String
makefile name dirBase dirAbsyn absynFileNames =
    unlines
    [
     "JAVAC = javac",
     "JAVAC_FLAGS = -sourcepath .",
     "",
     "JAVA = java",
     "",
     "CUP = java_cup.Main",
     "CUPFLAGS = -nopositions -expect 100",
     "",
     "JLEX = JLex.Main",
     "",
     "LATEX = latex",
     "DVIPS = dvips",
     "",
     "all: test " ++ name ++ ".ps",
     "",
     "test: absyn "
       ++ unwords (map (dirBase ++) ["Visitor.class",
				      "Visitable.class",
				      "Test.class"]),
     "",
     ".PHONE: absyn",
     "",
     "absyn: " ++ absynJavaClass,
     "",
     "%.class: " ++ "%.java",
     "\t${JAVAC} ${JAVAC_FLAGS} $*.java",
     "",
     dirBase ++ "Visitable.class: " ++ dirBase ++ "Visitable.java",
     "\t${JAVAC} ${JAVAC_FLAGS} " ++ dirBase ++ "Visitable.java",
     "",
     dirBase ++ "Visitor.class: " ++ dirBase ++ "Visitor.java",
     "\t${JAVAC} ${JAVAC_FLAGS} " ++ dirBase ++ "Visitor.java",
     "",
     dirBase ++ "Yylex.java: " ++ dirBase ++ "Yylex",
     "\t${JAVA} ${JLEX} " ++ dirBase ++ "Yylex",
     "",
     -- FIXME
     dirBase ++ "sym.java" ++ " " ++ dirBase ++ "parser.java: " ++ dirBase ++ name ++ ".cup",
     "\t${JAVA} ${CUP} ${CUPFLAGS} " ++ dirBase ++ name ++ ".cup ; mv sym.java parser.java " ++ dirBase,
     "",
     dirBase ++ "Yylex.class: " ++ dirBase ++ "Yylex.java "
       ++ dirBase ++ "sym.java",
     "\t${JAVAC} ${JAVAC_FLAGS} " ++ dirBase ++ "Yylex.java",
     "",
     dirBase ++ "Test.class: "
       ++ unwords (map (dirBase ++) ["Test.java",
				     "PrettyPrinter.class",
				     "Yylex.class",
				     "parser.class",
				     "sym.class"]),
     "\t${JAVAC} ${JAVAC_FLAGS} " ++ dirBase ++ "Test.java",
     "",
     "" ++ name ++ ".dvi: " ++ name ++ ".tex",
     "\t${LATEX} " ++ name ++ ".tex",
     "",
     "" ++ name ++ ".ps: " ++ name ++ ".dvi",
     "\t${DVIPS} " ++ name ++ ".dvi -o " ++ name ++ ".ps",
-- FIXME
     "",
     "clean:",
     "\t rm -f " ++ absynJavaClass,
     "\t rm -f " ++ ".dvi " ++ name ++ ".aux " ++ name ++ ".log " ++ name ++ ".ps " ++ " *.class Makefile",
     "",
     "distclean:",
     "\t rm -f " ++ absynJavaSrc ++ " " ++ absynJavaClass,
     "\t rmdir " ++ dirAbsyn,
     "\t rm -f " ++ name ++ ".tex " ++ name ++ ".dvi " ++ name ++ ".aux " ++ name ++ ".log " ++ name ++ ".ps ",
     "\t rm -f " ++ unwords (map (dirBase ++) ["Yylex",
					       name ++ ".cup",
					       "Yylex.java",
					       "sym.java",
					       "parser.java",
					       "Visitor.java",
					       "Visitable.java",
					       "VisitSkel.java",
					       "PrettyPrinter.java",
					       "Skeleton.java",
					       "Test.java",
					       "*.class"]),
     "\t rmdir " ++ dirBase,
     "\t rm -f Makefile",
     ""
    ]
    where absynJavaSrc = unwords (map (++ ".java") absynFileNames)
	  absynJavaClass = unwords (map (++ ".class") absynFileNames)

prVisitable :: String -> String
prVisitable packageBase = unlines
 [
  "package" +++ packageBase ++ ";\n",
  "public interface Visitable",
  "{",
  "  public void accept(" ++ packageBase ++ ".Visitor v);",
  "}"
 ]

prVisitor :: String -> String -> [String] -> [String]-> String
prVisitor packageBase packageAbsyn funs user =
    unlines
    [
     "package" +++ packageBase ++ ";\n",
     "public interface Visitor",
     "{",
     concatMap prVisitFun funs,
     concatMap prUser user,
     footer
    ]
 where
   prUser u = "  public void visit" ++ u' ++ "(String p);\n"
    where
     u' = ((toUpper (head u)) : (map toLower (tail u))) --this is a hack to fix a potential capitalization problem.
   footer = unlines
    [  --later only include used categories
     "  public void visitIdent(String i);",
     "  public void visitInteger(Integer i);",
     "  public void visitDouble(Double d);",
     "  public void visitChar(Character c);",
     "  public void visitString(String s);",
     "}"
    ]
   prVisitFun f = "  public void visit" ++ f ++ "(" ++ packageAbsyn
		       ++ "." ++ f ++ " p);\n"

javaTest :: String -> String -> CF -> String
javaTest packageBase packageAbsyn cf =
    unlines
    [
     "package" +++ packageBase ++ ";",
     "import java_cup.runtime.*;",
     "import" +++ packageBase ++ ".*;",
     "import" +++ packageAbsyn ++ ".*;",
     "import java.io.*;",
     "",
     "public class Test",
     "{",
     "  public static void main(String args[]) throws Exception",
     "  {",
     "    Yylex l = null;",
     "    parser p;",
     "    try",
     "    {",
     "      if (args.length == 0) l = new Yylex(System.in);",
     "      else l = new Yylex(new FileReader(args[0]));",
     "    }",
     "    catch(FileNotFoundException e)",
     "    {",
     "     System.err.println(\"Error: File not found: \" + args[0]);",
     "     System.exit(1);",
     "    }",
     "    p = new parser(l);",
     "    /* The default parser is the first-defined entry point. */",
     "    /* You may want to change this. Other options are: */",
     "    /* " ++ (concat (intersperse ", " (showOpts (tail eps)))) ++ " */",
     "    try",
     "    {",
     "      " ++ packageAbsyn ++ "." ++ def +++ "parse_tree = p.p"
     ++ def ++ "();",
     "      System.out.println();",
     "      System.out.println(\"Parse Succesful!\");",
     "      System.out.println();",
     "      System.out.println(\"[Abstract Syntax]\");",
     "      System.out.println();",
     "      System.out.println(PrettyPrinter.show(parse_tree));",
     "      System.out.println();",
     "      System.out.println(\"[Linearized Tree]\");",
     "      System.out.println();",
     "      System.out.println(PrettyPrinter.print(parse_tree));",
     "    }",
     "    catch(Throwable e)",
     "    {",
     "      System.err.println(\"At line \" + String.valueOf(l.line_num()) + \", near \\\"\" + l.buff() + \"\\\" :\");",
     "      System.err.println(\"     \" + e.getMessage());",
     "      System.exit(1);",
     "    }",
     "  }",
     "}"
    ]
    where eps = allEntryPoints cf
	  def = head eps
	  showOpts [] = []

	  showOpts (x:[]) = if normCat x /= x then [] else ['p' : (identCat x)]
	  showOpts (x:xs) = if normCat x /= x then (showOpts xs)
			    else ('p' : (identCat x)) : (showOpts xs)

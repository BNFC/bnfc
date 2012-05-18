{-
    BNF Converter: Java Top File
    Copyright (C) 2004  Author:  Markus Forsberg, Peter Gammie, 
                                 Michael Pellauer, Bjorn Bringert

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
-- Copyright   :  (C)opyright 2003, {markus, aarne, pellauer, peteg, bringert} at cs dot chalmers dot se
-- License     :  GPL (see COPYING for details)
-- 
-- Maintainer  :  {markus, aarne} at cs dot chalmers dot se
-- Stability   :  alpha
-- Portability :  Haskell98
--
-- Top-level for the Java back end.
--
-- > $Id: JavaTop15.hs,v 1.12 2007/01/08 18:20:23 aarne Exp $
-------------------------------------------------------------------

module JavaTop15 ( makeJava15 ) where 

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------
import System.Directory	( createDirectory )
import System.IO.Error	( try, isAlreadyExistsError )
import System.Exit      ( exitFailure )
import Utils
import CF
import CFtoCup15       	( cf2Cup )
import CFtoJLex15
import CFtoJavaAbs15	( cf2JavaAbs )
import CFtoJavaPrinter15
--import CFtoJavaSkeleton
import CFtoVisitSkel15
import CFtoComposVisitor
import CFtoAbstractVisitor
import CFtoFoldVisitor
import CFtoAllVisitor
import CFtoLatex
import GetCF		( tryReadCF )
import Data.Char
import Data.List(intersperse)

-------------------------------------------------------------------
-- | Build the Java output.
-- FIXME: get everything to put the files in the right places.
-- Adapt Makefile to do the business.
-------------------------------------------------------------------
makeJava15 :: Bool 
	  -> Maybe String -- ^ Java package name to put the classes in
	  -> String -- ^ Name of grammar
	  -> FilePath -- ^ Grammar file
	  -> IO ()
makeJava15 make inPackage name file =
    do (cf, isOK) <- tryReadCF file
       if isOK
         then do mkFiles make inPackage name cf
		 putStrLn $ "Done!"
	 else do putStrLn $ "Failed!"
		 exitFailure

mkFiles :: Bool -> Maybe String -> String -> CF -> IO ()
mkFiles make inPackage name cf =
    do -- Create the package directories if necessary.
       let packageBase = case inPackage of
                             Nothing -> name
                             Just p -> p ++ "." ++ name
	   packageAbsyn = packageBase ++ "." ++ "Absyn"
	   dirBase = pkgToDir packageBase
	   dirAbsyn = pkgToDir packageAbsyn
       prepareDir dirBase
       prepareDir dirAbsyn
       let absynFiles = remDups $ cf2JavaAbs packageBase packageAbsyn cf
	   absynBaseNames = map fst absynFiles
	   absynFileNames = map (dirAbsyn ++) absynBaseNames
	   absynFuns = [ f | (_,ps) <- cf2data cf, (f,_) <- ps ]
       let writeAbsyn (filename, contents) =
	       writeFileRep (dirAbsyn ++ filename ++ ".java") contents
       mapM writeAbsyn absynFiles
       writeFileRep (dirBase ++ "PrettyPrinter.java") $ cf2JavaPrinter packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "VisitSkel.java") $ cf2VisitSkel packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "ComposVisitor.java") $ cf2ComposVisitor packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "AbstractVisitor.java") $ cf2AbstractVisitor packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "FoldVisitor.java") $ cf2FoldVisitor packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "AllVisitor.java") $ cf2AllVisitor packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "Test.java") $ javaTest packageBase packageAbsyn cf
       writeFileRep (dirBase ++ "Test" ++ name) $ "java Test $(1)"
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
      pkgToDir s = replace '.' pathSep s ++ [pathSep]


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
     "JAVA_FLAGS =",
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
       ++ unwords (map (dirBase ++) [
				      "Yylex.class",
				      "PrettyPrinter.class",
				      "Test.class",
                                      "ComposVisitor.class",
                                      "AbstractVisitor.class",
                                      "FoldVisitor.class",
                                      "AllVisitor.class",
				      "parser.class",
				      "sym.class",
				      "Test.class"]),
     "",
     ".PHONY: absyn",
     "",
     "%.class: %.java",
     "\t${JAVAC} ${JAVAC_FLAGS} $^",
     "",
     "absyn: " ++ absynJavaSrc,
     "\t${JAVAC} ${JAVAC_FLAGS} $^",
     "",
     dirBase ++ "Yylex.java: " ++ dirBase ++ "Yylex",
     "\t${JAVA} ${JAVA_FLAGS} ${JLEX} " ++ dirBase ++ "Yylex",
     "",
     dirBase ++ "sym.java " ++ dirBase ++ "parser.java: " ++ dirBase ++ name ++ ".cup",
     "\t${JAVA} ${JAVA_FLAGS} ${CUP} ${CUPFLAGS} " ++ dirBase ++ name ++ ".cup",
     "\tmv sym.java parser.java " ++ dirBase,
     "",
     dirBase ++ "Yylex.class: " ++ dirBase ++ "Yylex.java"
       ++ " " ++ dirBase ++ "sym.java",
     "",
     dirBase ++ "sym.class: " ++ dirBase ++ "sym.java",
     "",
     dirBase ++ "parser.class: " ++ dirBase ++ "parser.java " ++ dirBase ++ "sym.java",
     "",
     dirBase ++ "PrettyPrinter.class: " ++ dirBase ++ "PrettyPrinter.java",
     "",
     "" ++ name ++ ".dvi: " ++ name ++ ".tex",
     "\t${LATEX} " ++ name ++ ".tex",
     "",
     "" ++ name ++ ".ps: " ++ name ++ ".dvi",
     "\t${DVIPS} " ++ name ++ ".dvi -o " ++ name ++ ".ps",
-- FIXME
     "",
     "clean:",
     "\t rm -f " ++ dirAbsyn ++ "*.class" ++ " " ++ dirBase ++ "*.class",
     "\t rm -f " ++ ".dvi " ++ name ++ ".aux " ++ name ++ ".log " ++ name ++ ".ps " ++ " *.class",
     "",
     "distclean: vclean",
     "",
     "vclean:",
     "\t rm -f " ++ absynJavaSrc ++ " " ++ absynJavaClass,
     "\t rm -f " ++ dirAbsyn ++ "*.class " ++ dirBase ++ "Test" ++ name,
     "\t rmdir " ++ dirAbsyn,
     "\t rm -f " ++ name ++ ".tex " ++ name ++ ".dvi " ++ name ++ ".aux " ++ name ++ ".log " ++ name ++ ".ps ",
     "\t rm -f " ++ unwords (map (dirBase ++) [
					        "Yylex",
						name ++ ".cup",
						"Yylex.java",
						"VisitSkel.java",
                                                "ComposVisitor.java",
                                                "AbstractVisitor.java",
                                                "FoldVisitor.java",
                                                "AllVisitor.java",
						"PrettyPrinter.java",
						"Skeleton.java",
						"Test.java",
						"sym.java",
						"parser.java",
						"*.class"]),
     "\t rm -f Makefile",
     "\t rmdir -p " ++ dirBase,
     ""
    ]
    where absynJavaSrc = unwords (map (++ ".java") absynFileNames)
	  absynJavaClass = unwords (map (++ ".class") absynFileNames)

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

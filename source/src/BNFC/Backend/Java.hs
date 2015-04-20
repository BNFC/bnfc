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

module BNFC.Backend.Java ( makeJava ) where

-------------------------------------------------------------------
-- Dependencies.
-------------------------------------------------------------------
import System.FilePath (pathSeparator)
import BNFC.Utils
import BNFC.CF
import BNFC.Options as Options
import BNFC.Backend.Base
import BNFC.Backend.Java.CFtoCup15 ( cf2Cup )
import BNFC.Backend.Java.CFtoJLex15
import BNFC.Backend.Java.CFtoJavaAbs15 ( cf2JavaAbs )
import BNFC.Backend.Java.CFtoJavaPrinter15
import BNFC.Backend.Java.CFtoVisitSkel15
import BNFC.Backend.Java.CFtoComposVisitor
import BNFC.Backend.Java.CFtoAbstractVisitor
import BNFC.Backend.Java.CFtoFoldVisitor
import BNFC.Backend.Java.CFtoAllVisitor
import qualified BNFC.Backend.Common.Makefile as Makefile
import BNFC.PrettyPrint
-------------------------------------------------------------------
-- | Build the Java output.
-- FIXME: get everything to put the files in the right places.
-- Adapt Makefile to do the business.
-------------------------------------------------------------------
makeJava :: SharedOptions -> CF -> MkFiles ()
makeJava options@Options{..} cf =
    do -- Create the package directories if necessary.
       let packageBase = case inPackage of
                             Nothing -> lang
                             Just p -> p ++ "." ++ lang
           packageAbsyn = packageBase ++ "." ++ "Absyn"
           dirBase = pkgToDir packageBase
           dirAbsyn = pkgToDir packageAbsyn
       let absynFiles = remDups $ cf2JavaAbs packageBase packageAbsyn cf
           absynBaseNames = map fst absynFiles
           absynFileNames = map (dirAbsyn ++) absynBaseNames
       let writeAbsyn (filename, contents) =
               mkfile (dirAbsyn ++ filename ++ ".java") contents
       mapM_ writeAbsyn absynFiles
       mkfile (dirBase ++ "PrettyPrinter.java") $ cf2JavaPrinter packageBase packageAbsyn cf
       mkfile (dirBase ++ "VisitSkel.java") $ cf2VisitSkel packageBase packageAbsyn cf
       mkfile (dirBase ++ "ComposVisitor.java") $ cf2ComposVisitor packageBase packageAbsyn cf
       mkfile (dirBase ++ "AbstractVisitor.java") $ cf2AbstractVisitor packageBase packageAbsyn cf
       mkfile (dirBase ++ "FoldVisitor.java") $ cf2FoldVisitor packageBase packageAbsyn cf
       mkfile (dirBase ++ "AllVisitor.java") $ cf2AllVisitor packageBase packageAbsyn cf
       mkfile (dirBase ++ "Test.java") $ render $ javaTest packageBase packageAbsyn cf
---       mkfile ("Test" ++ name) $ "java " ++ dirBase ++ "Test $(1)"
       let (lex, env) = cf2jlex packageBase cf jflex
       mkfile (dirBase ++ "Yylex") (render lex)
       liftIO $ putStrLn "   (Tested with JLex 1.2.6.)"
       mkfile (dirBase ++ lang ++ ".cup") $ cf2Cup packageBase packageAbsyn cf env
       -- FIXME: put in a doc directory?
       liftIO $ putStrLn $ "   (Parser created for category " ++ show (firstEntry cf) ++ ")"
       liftIO $ putStrLn "   (Tested with CUP 0.10k)"
       Makefile.mkMakefile options $ makefile lang dirBase dirAbsyn absynFileNames jflex
    where
      remDups [] = []
      remDups ((a,b):as) = case lookup a as of
                             Just {} -> remDups as
                             Nothing -> (a, b) : remDups as

      pkgToDir :: String -> FilePath
      pkgToDir s = replace '.' pathSeparator s ++ [pathSeparator]

-- FIXME get filenames right.
-- FIXME It's almost certainly better to just feed all the Java source
-- files to javac in one go.
-- Replace with an ANT script?
makefile :: String -> FilePath -> FilePath -> [String] -> Bool -> String
makefile name dirBase dirAbsyn absynFileNames jflex =
    Makefile.mkVar "JAVAC" "javac"
  $ Makefile.mkVar "JAVAC_FLAGS" "-sourcepath ."
  $ Makefile.mkVar "JAVA" "java"
  $ Makefile.mkVar "JAVA_FLAGS" ""
  $ Makefile.mkVar "CUP" "java_cup.Main"
  $ Makefile.mkVar "CUPFLAGS" "-nopositions -expect 100"
  $ (if jflex then Makefile.mkVar "JFLEX" "jflex"
              else Makefile.mkVar "JLEX" "JLex.Main" )
  $ Makefile.mkRule "all" [ "test" ]
    []
  $ Makefile.mkRule "test" ("absyn" : map (dirBase ++) [ "Yylex.class",
                                                       "PrettyPrinter.class",
                                                       "Test.class",
                                                       "ComposVisitor.class",
                                                       "AbstractVisitor.class",
                                                       "FoldVisitor.class",
                                                       "AllVisitor.class",
                                                       "parser.class",
                                                       "sym.class",
                                                       "Test.class"])
    []
  $ Makefile.mkRule ".PHONY" ["absyn"]
    []
  $ Makefile.mkRule "%.class" [ "%.java" ]
    [ "${JAVAC} ${JAVAC_FLAGS} $^" ]
  $ Makefile.mkRule "absyn" [absynJavaSrc]
    [ "${JAVAC} ${JAVAC_FLAGS} $^" ]
  $ Makefile.mkRule (dirBase ++ "Yylex.java") [ dirBase ++ "Yylex" ]
    [ (if jflex then "${JFLEX} " else "${JAVA} ${JAVA_FLAGS} ${JLEX} ") ++ dirBase ++ "Yylex" ]
  $ Makefile.mkRule (dirBase ++ "sym.java " ++ dirBase ++ "parser.java")
                    [ dirBase ++ name ++ ".cup" ]
    [ "${JAVA} ${JAVA_FLAGS} ${CUP} ${CUPFLAGS} " ++ dirBase ++ name ++ ".cup"
    , "mv sym.java parser.java " ++ dirBase ]
  $ Makefile.mkRule (dirBase ++ "Yylex.class") [ dirBase ++ "Yylex.java",
                                                 dirBase ++ "sym.java" ]
    []
  $ Makefile.mkRule (dirBase ++ "sym.class") [ dirBase ++ "sym.java" ]
    []
  $ Makefile.mkRule (dirBase ++ "parser.class") [ dirBase ++ "parser.java"
                                                , dirBase ++ "sym.java" ]
    []
  $ Makefile.mkRule (dirBase ++ "PrettyPrinter.class")
                    [ dirBase ++ "PrettyPrinter.java" ]
    []
-- FIXME
  $ Makefile.mkRule "clean" []
    [ "rm -f " ++ dirAbsyn ++ "*.class" ++ " " ++ dirBase ++ "*.class" ]
  $ Makefile.mkRule "distclean" [ "vclean" ]
    []
  $ Makefile.mkRule "vclean" []
    [ " rm -f " ++ absynJavaSrc ++ " " ++ absynJavaClass
    , " rm -f " ++ dirAbsyn ++ "*.class"
--     , "rm -f " ++ "Test" ++ name
    , " rmdir " ++ dirAbsyn
    , " rm -f " ++ unwords (map (dirBase ++) [
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
                                                "*.class"])
    , "rm -f Makefile"
    , "rmdir -p " ++ dirBase ]
  ""
    where absynJavaSrc = unwords (map (++ ".java") absynFileNames)
          absynJavaClass = unwords (map (++ ".class") absynFileNames)

javaTest :: String -> String -> CF -> Doc
javaTest packageBase packageAbsyn cf = vcat
    [ "package" <+> text packageBase <> ";"
    , "import java_cup.runtime.*;"
    , "import" <+> text packageBase <> ".*;"
    , "import" <+> text packageAbsyn <> ".*;"
    , "import java.io.*;"
    , ""
    , "public class Test"
    , codeblock 2
        [ "public static void main(String args[]) throws Exception"
        , codeblock 2
            [ "Yylex l = null;"
            , "parser p;"
            , "try"
            , codeblock 2
                [ "if (args.length == 0) l = new Yylex(new InputStreamReader(System.in));"
                , "else l = new Yylex(new FileReader(args[0]));" ]
            , "catch(FileNotFoundException e)"
            , "{"
            , " System.err.println(\"Error: File not found: \" + args[0]);"
            , " System.exit(1);"
            , "}"
            , "p = new parser(l);"
            , "/* The default parser is the first-defined entry point. */"
            , "/* You may want to change this. Other options are: */"
            , "/* " <> fsep (punctuate "," (showOpts (tail eps))) <> " */"
            , "try"
            , "{"
            , "  " <> text packageAbsyn <> "." <> text (show def) <+> "parse_tree = p.p"
             <> text (show def) <> "();"
            , "  System.out.println();"
            , "  System.out.println(\"Parse Succesful!\");"
            , "  System.out.println();"
            , "  System.out.println(\"[Abstract Syntax]\");"
            , "  System.out.println();"
            , "  System.out.println(PrettyPrinter.show(parse_tree));"
            , "  System.out.println();"
            , "  System.out.println(\"[Linearized Tree]\");"
            , "  System.out.println();"
            , "  System.out.println(PrettyPrinter.print(parse_tree));"
            , "}"
            , "catch(Throwable e)"
            , "{"
            , "  System.err.println(\"At line \" + String.valueOf(l.line_num()) + \", near \\\"\" + l.buff() + \"\\\" :\");"
            , "  System.err.println(\"     \" + e.getMessage());"
            , "  System.exit(1);"
            , "}"
            ]
        ]
    ]
  where
    eps = allEntryPoints cf
    def = head eps
    showOpts [] = []
    showOpts (x:xs) | normCat x /= x = showOpts xs
                    | otherwise      = text ('p' : identCat x) : showOpts xs

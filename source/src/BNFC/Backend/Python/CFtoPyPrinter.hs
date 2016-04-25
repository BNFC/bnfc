{-
    BNF Converter: Python PyParsing abstract syntax classes.
    Copyright (C) 2015  Author:  Gabriele Paganelli

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

{-
   **************************************************************
    BNF Converter Module

    Description   : This module generates a file containing a 
                    collection of Python classes representing
                    the abstract syntax of the language being 
                    implemented.

    Author        : Gabriele Paganelli (gapag@distruzione.org)

    License       : GPL (GNU General Public License)

    Created       : 28 Jan, 2016

    Modified      :


   **************************************************************
-}

module BNFC.Backend.Python.CFtoPyPrinter ( cf2PyPrinter ) where

import BNFC.CF
import BNFC.Backend.Python.Utils
import BNFC.Backend.Python.AbsPython
import BNFC.Backend.Common.StrUtils
import BNFC.Backend.Common.NamedVariables
import Data.Either (lefts)
import Text.PrettyPrint

cf2PyPrinter :: String -> CF -> String
cf2PyPrinter _ cf = show $ absVcat entities
                    where
                        entities = concat [
                                        [From $ Ident "absyn"]
                                        , prettyPrintingEntryClass
                                        , prettyPrinterClass cf]
                            
                   
            
                                


i :: Entity
i = mkId "i"

pprender :: Entity
pprender = mkId "render"

buf_  :: Entity
buf_ = toNames [Self, buf_Id] 

ppEntry_s :: String
ppEntry_s = "PPEntry"

ppEntry :: Entity
ppEntry = mkId ppEntry_s

prettyPrintingEntryClass :: [Entity]
prettyPrintingEntryClass = 
    [Class (Ident  ppEntry_s) NoInherit
    , methodDefinition Init [Self, shId, pp] body 
    ]
    where 
        body = assigningConstructorBody ["sh","pp"]
        
                        
callRender x = Function (toNames [Self, pp, pprender]) [x]

prettyPrinterClass :: CF -> [Entity]
prettyPrinterClass cf = 
    [ Class (Ident "PrettyPrinter") NoInherit] ++
                     concat [ initDef cf
                            , renderDef
                            , pprintDef
                            , showDef
                            , __list_ppDef
                            , __list_shDef
                            , __pp_def cf
                            , __sh_def cf
                            , shDef 
                            , ppDef 
                            , printQuotedDef 
                            , indentDef 
                            , backupDef 
                            , trimDefDef 
                            ]    



initDef :: CF -> [Entity]
initDef cf = [
    classMethodDefinition Init [] $
        
            (a [_l_paren, _r_paren, n, indentWidth, buf_] 
                ["'('", "')'", "0", "2", "[]"]) 
            ++ [Dictionary (dictionary printingVisitors absynCats)]
            
    ]
    where
        a lhs rhs = [Assignment [l_i] [mkId r_i] | (l_i, r_i) <- zip lhs rhs]
        absynCats = getUserCategories cf

printingVisitors :: Fun -> Entity
printingVisitors f =  Entry (mkId f) (ppEntryCons f)

ppEntryCons f = Function ppEntry $ map iv [privateShName,privatePpName ]
    where 
        iv x = instVar (x f)
 
privateShName :: Fun -> String
privateShName x = "__sh_"++x

privatePpName :: Fun -> String
privatePpName x = "__pp_"++x

-- self.dict = {
--             Db: PPEntry(self.__sh_Db, self.__pp_Db),
--             Fact: PPEntry(self.__sh_Fact, self.__pp_Fact),
--             Rule: PPEntry(self.__sh_Rule, self.__pp_Rule),
--             Directive: PPEntry(self.__sh_Directive, self.__pp_Directive),
--             APred: PPEntry(self.__sh_APred, self.__pp_APred),
--             CPred: PPEntry(self.__sh_CPred, self.__pp_CPred),
--             TAtom: PPEntry(self.__sh_TAtom, self.__pp_TAtom),
--             VarT: PPEntry(self.__sh_VarT, self.__pp_VarT),
--             Complex: PPEntry(self.__sh_Complex, self.__pp_Complex),
--             TList: PPEntry(self.__sh_TList, self.__pp_TList),
--             Atm: PPEntry(self.__sh_Atm, self.__pp_Atm),
--             EAtm: PPEntry(self.__sh_EAtm, self.__pp_EAtm),
--             V: PPEntry(self.__sh_V, self.__pp_V),
--             A: PPEntry(self.__sh_A, self.__pp_A),
--             Empty: PPEntry(self.__sh_Empty, self.__pp_Empty),
--             Enum: PPEntry(self.__sh_Enum, self.__pp_Enum),
--             Cons: PPEntry(self.__sh_Cons, self.__pp_Cons),
--             ConsV: PPEntry(self.__sh_ConsV, self.__pp_ConsV),
--         }
--         # For certain applications increasing the initial size of the buffer may improve performance.
-- 
--     
--     INDENT_WIDTH = 2
--     # You may wish to change the parentheses used in precedence.
--     _L_PAREN = "("
--     _R_PAREN = ")"
--     buf_ = []
--     _n_ = 0

renderDef :: [Entity]
renderDef = [
    classMethodDefinition renderId [s] $
        ifElseCascade [
            (charTest ["{"], openBraceAction),
            (charTest ["(", "["], openParAction),
            (charTest [")", "]"], closeParAction),
            (charTest ["}"], closeBraceAction),
            (charTest [","], commaAction),
            (charTest [";"], semicolonAction),
            (charTest [""], emptyStringAction)
        ]
        [
            callAppend [s],
            callAppend [whiteSpaceChar]
        ]
    ]
    where
        charTest [c] = Equals s $ mkStr c
        charTest c = In s $ tupleLiteral $ map mkStr c
        mkStr x =  mkId $ "\""++x++"\""
        openBraceAction = [
            newline 
            , indent
            , callAppend [s]
            , Assignment [n] [Plus n indentWidth]
            , newline
            , indent
            ]
        openParAction = [
            callAppend [s]
            ]
        closeParAction = [
            backup
            , callAppend [s]
            , space
            ]
        closeBraceAction = [
            Assignment [n] [Minus n indentWidth]
            , For [t] rng 
            , IndentedBlock [
                backup
                ]
            , callAppend [s]
            , newline
            , indent
            ]
        commaAction = [
            backup
            , callAppend [s]
            , space
            ]
        semicolonAction = [
            backup
            , callAppend [s]
            , newline
            , indent
            ]
        emptyStringAction = [Return $ mkId "None"]
        newline = callAppend  [mkId "'\\n'"]
        space = callAppend [whiteSpaceChar]
        indent = callTo indentId []
        backup = callTo backupId []
        t = mkId "t"
        rng = Function (mkId "range") [ mkId "0",indentWidth]


joinAndClear :: [Entity]
joinAndClear = [Assignment [s] [Function join [buf_]]
                , Function (toNames [buf_, mkId "clear"]) []
                , Return s]
            where
                join = toNames $ map mkId ["\"\"","join"]


pprintDef :: [Entity]
pprintDef = [
    classMethodDefinition pprintId [s] ([
            lookupFun $ Function pp [s, mkId "0"]
            , callTo trimId []
        ] ++ joinAndClear)
    ]

showDef :: [Entity]
showDef = [
    classMethodDefinition (mkId "show") [s] ([
            lookupFun $ Function shId [s]] 
            ++ joinAndClear)
    ]

__list_ppId = mkId "__list_pp"
__list_ppDef :: [Entity]
__list_ppDef = [
    classMethodDefinition __list_ppId [sep, li, i] [
        Assignment [l] [Minus (Function len [li]) (mkId "1")]
        , For [idx, k] $ enumerate li
        , IndentedBlock ([callTo pp [k, i]]++chooseSepOrTerminator)
        ]
    ]
    where
        sepId = "sep"
        [l, len, sep, li, idx, k] = map mkId ["l", "len", sepId, "li", "idx", "k"]
        chooseSepOrTerminator = ifElseCascade 
                                [(NoEquals idx l, [renderSep 0])] 
                                [renderSep 1]
        renderSep x = callToRender [mkAccess sepId x] 

__list_shId = mkId "__list_sh"
__list_shDef :: [Entity]
__list_shDef = [
    classMethodDefinition __list_shId [li] [
        Assignment [l] [Minus (Function len [li]) (mkId "1")]
        , For [idx, k] $ enumerate li
        , IndentedBlock $ [callTo shId [k]] ++ conditionalComma
        ]
    ]
    where
        [len, li, idx, k, l] = map mkId ["len", "li", "idx", "k", "l"]
        conditionalComma = ifCascade [
                                (NoEquals idx l 
                                ,[callToRender [mkId "','"]])
                                ]


-- Should call  and push a Cat -> [RuleP] function?
__pp_def :: CF -> [Entity]
__pp_def cf = concatMap (ppMethodSet $ rulesForCat cf) groups
    where
        groups = fixCoercions (ruleGroupsInternals cf)

__sh_def :: CF -> [Entity]
__sh_def cf = concatMap (shMethodSet $ rulesForNormalizedCat cf) groups
    where
        groups = fixCoercions (ruleGroupsInternals cf)

printOrShowMethodSet :: PrintingMethod -> CatDefs -> (Cat, [Rule]) -> [Entity]
printOrShowMethodSet pm defs (_, rules) = concatMap (pm defs) rules 

shMethodSet :: CatDefs -> (Cat, [Rule]) -> [Entity]
shMethodSet defs (cat, rules) = printOrShowMethodSet shMethod defs (cat, rules)

type PrintingMethod = CatDefs -> Rule -> [Entity]

ppMethodSet :: CatDefs -> (Cat, [Rule]) -> [Entity]
ppMethodSet defs (cat, rules) = printOrShowMethodSet ppMethod defs (cat, rules)

fParam = mkId "p"

ppMethod :: CatDefs ->Rule -> [Entity]
ppMethod defs (Rule fun _c cats) | not (isCoercion fun || isDefinedRule fun || isNilCons fun) =  ruleMethod
  where    
    ruleMethod = [ classMethodDefinition fname [fParam, i] nonTerminalContributions]
    fname = mkId $ privatePpName fun
    nonTerminalContributions = case cats of
        [] -> [Pass]
        _  ->   lp ++ content ++ rp
                
    content = map (ppMethodStatement defs) (numVars cats)
    lp = ifpar _l_paren
    rp = ifpar _r_paren
    ifpar x = ifCascade [(Gt i (mkId ( show $ precCat _c)), [callRender x])]
    
ppMethod _ _nm = [NothingPython]

ppMethodStatement :: CatDefs -> Either (Cat, Doc) String -> Entity
ppMethodStatement _ (Right t) = callToRender [mkId $ "\""++ (escapeChars t) ++"\""]
ppMethodStatement _ (Left (TokenCat "String", nt)) 
    = callTo printQuotedId [ toNames $ map mkId ["p" , escapeChars$ render nt]]
ppMethodStatement _ (Left (InternalCat, _)) = Pass
ppMethodStatement catdefs (Left (cat, nt)) = if isList cat 
    then callTo __list_ppId [separatorTuple, child, precedence ]
    else callTo pp [child, precedence]
    where
        child = toNames [pid , ntid]
        separatorTuple = tupleLiteral [sep, term]
        precedence = mkId ( show $ precCat cat)
        [pid,ntid, sep, term] = map mkId ["p", render nt, ssator, stator]
        rules = catdefs cat
        [ssator, stator] = map (\x -> "\""++ x ++"\"") [sator, tator]
        (sator, tator) = (escapeChars $ getCons rules
                         , if hasOneFunc rules then "" else sator)

shMethod :: CatDefs ->Rule -> [Entity]
shMethod defs (Rule fun _c cats) | not (isCoercion fun || isDefinedRule fun || isNilCons fun) =  ruleMethod
  where    
    ruleMethod = [ classMethodDefinition fname [fParam] methodBody]
    fname = mkId $ privateShName fun
    methodBody = [
        lparen
        ,ctr $ "\""++fun++"\""
        ]++nonTerminalContributions++[rparen]
    nonTerminalContributions = case cats of
        [] -> [NothingPython]
        _  -> concatMap (shMethodStatement defs) ( lefts (numVars cats))
    (lparen,rparen) = if allTerms cats
        then (NothingPython, NothingPython)
        else (ctr "'('",ctr "')'")
    ctr x = callToRender [mkId x]
    allTerms [] = True
    allTerms ((Left {}):_) = False
    allTerms (_:zs) = allTerms zs
shMethod _ _nm = [NothingPython]


shMethodStatement :: CatDefs -> (Cat, Doc) -> [Entity]
-- shMethodStatement _ (Right t) = [callToRender [mkId $ "\""++ t ++"\""]]
-- shMethodStatement _ (Left (TokenCat "String", nt)) 
--     = [callTo printQuotedId [ toNames $ map mkId ["p" , render nt]]]
shMethodStatement _ (InternalCat, _) = [Pass]
shMethodStatement _ (cat, nt) = if isList cat 
    then [
        callToRender [mkId "'['"]
        , callTo __list_shId [child]
        ,callToRender [mkId "']'"]
        ]
    else [callTo shId [child]]
    where
        child = toNames [pid , ntid]
        [pid,ntid] = map mkId ["p", render nt]

isInstance :: Entity -> Entity
isInstance x = Function (mkId "isinstance") [x, str]

enumerate :: Entity -> Entity
enumerate x = Function (mkId "enumerate") [x]

shDef :: [Entity]
shDef = [
        classMethodDefinition shId [s] [
            Try,
            IndentedBlock [
                Function lookupObject [s]
                ],
            Except,
            IndentedBlock $ 
                ifElseCascade
                        [(isInstance s,[callTo printQuotedId [s]]) ]
                        [callToRender $ [Function str [s]]]
            
        ]
    ]
    where
        lookupObject = lookupFun shId


lookupFun fun = toNames [Self, dictionaryLookup s, fun]

str :: Entity
str = mkId "str"

callToRender :: [Entity] -> Entity
callToRender x = callTo renderId x

ppDef :: [Entity]
ppDef = [
        classMethodDefinition pp [s, i] [
            Try,
            IndentedBlock [
                Assignment [fu] [di],
                Function fupp [s, i] 
                
            ],
            Except,
            IndentedBlock [
                callAppend [Function str [s]],
                callAppend [whiteSpaceChar]
            ]
                
            ]
        ]
    
    where 
        di = toNames [Self, dictionaryLookup s]
        fu = mkId "fu"
        fupp = toNames [fu, pp]
        
        
s :: Entity
s = mkId "s"
 
printQuotedDef :: [Entity]
printQuotedDef = [
        classMethodDefinition printQuotedId [s] [
            callTo renderId [Formatting "\"%s\"" s]
            ]
    ]
       
callAppend :: [Entity] -> Entity
callAppend x = callTo (toNames [buf_Id, appendId]) x

indentDef :: [Entity]
indentDef = [
            classMethodDefinition indentId [] [
                callAppend [timesSelfN]
            ]
        ]
        where
            timesSelfN = Mul whiteSpaceChar n
 
whiteSpaceChar , buf_Id, backupId, shId, endsWithId :: Entity
buf_Id = mkId "buf_"
backupId = mkId "backup"
shId  = mkId "sh"
endsWithId = mkId "endswith"
whiteSpaceChar = mkId "' '"
printQuotedId = mkId "printQuoted"
renderId = mkId "render"
indentId = mkId "indent"
appendId = mkId "append"
trimId = mkId "trim"
n = instVar "_n_"
indentWidth = instVar "INDENT_WIDTH"
_l_paren = instVar "_L_PAREN"
_r_paren = instVar "_R_PAREN"


backupDef :: [Entity]
backupDef = [
                classMethodDefinition backupId [] 
                        $ ifCascade [
                            ( 
                            Function (toNames [lhs, endsWithId]) [ whiteSpaceChar ],
                            [Assignment [lhs] [rhs]]
                            )
                        ]
                    
                ]
                where 
                    lhs = toNames [Self, mkAccess "buf_" (-1)]
                    rhs = SquareBracketAccess lhs $ YesArray $ mkId ":-1" 
                     
      
--     def backup(self):
--         if (self.buf_[-1].endswith(' ')):
--             self.buf_[-1] = self.buf_[-1][:-1]              
 
trimDefDef :: [Entity]
trimDefDef = [
            classMethodDefinition trimId [] [
                ass 0,
                ass (-1)
            ]
        ]
        where 
            lhs x = toNames [Self, mkAccess "buf_" x]
            rhs x = Function (toNames [lhs x, stripfun x])  []
            ass x = Assignment [lhs x] [rhs x] 
            stripfun x = case x of
                          -1 -> mkId "rstrip"
                          _  -> mkId "lstrip"
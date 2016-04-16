module BNFC.Backend.Python.Utils where
import BNFC.CF
import Data.Char (toLower)
import BNFC.Backend.Python.PrintPython as PPP
import BNFC.Backend.Python.AbsPython 
import Text.PrettyPrint as TPP
import BNFC.Backend.Common.NamedVariables
import qualified Data.Map as Dma

type CatDefs = (Cat -> [Rule])

indent = nest 4

importList li = vcat $ map ("import"<+>) li

mkId :: String -> Entity
mkId x = Id (Ident x)
 
mkAccess :: Show a => String -> a -> Entity 
mkAccess x y = SquareBracketAccess (mkId x) $ YesArray (mkId (show y)) 


dictionary :: (Fun -> Entity) -> [(Cat, [(Fun, [Cat])])]  -> [Entity]
dictionary _ [] = []
dictionary tycon ((_, labs):rest) = (entries funs)++(dictionary tycon rest)
                            where 
                                funs = fst $ unzip labs
                                entries = map tycon

dictionaryLookup :: Entity -> Entity
dictionaryLookup x= SquareBracketAccess dictionaryName $ YesArray $ lookupKey x

lookupKey :: Entity -> Entity
lookupKey x = toNames [x, ClassField]

toNames :: [Entity] -> Entity
toNames ent = Qualified [Name n | n <- ent]

assigningConstructorBody :: [String] -> [Entity]
assigningConstructorBody [] = [Pass]
assigningConstructorBody [aName] = 
    [Assignment [toNames [Self, i]] [i]]
        where i = mkId aName
assigningConstructorBody (name:s) =
    assigningConstructorBody [name] ++ assigningConstructorBody s
    
-- should return an indented block
methodDefinition :: Entity -> [Entity] -> [Entity] -> Entity
methodDefinition name args body =
    IndentedBlock [ Method $ Function name args,
        IndentedBlock body
        ]
        
classMethodDefinition :: Entity -> [Entity] -> [Entity] -> Entity
classMethodDefinition name ar body = methodDefinition name (concat [[Self], ar]) body


ifElseCascade :: [(Entity,[Entity])] -> [Entity] -> [Entity]
ifElseCascade [] [] = [Pass]
ifElseCascade branches [] = ifCascade branches
ifElseCascade branches elseBranch = ifCascade branches ++ e
    where 
        e = [ Else
            , IndentedBlock elseBranch
            ]

ifCascade :: [(Entity,[Entity])] -> [Entity]
ifCascade [] = [Pass]
ifCascade [(e,br)] =  [
    If e,
    IndentedBlock br
    ]
ifCascade (branch:es) = (ifCascade [branch]) ++ (elifCascade es)  

elifCascade :: [(Entity,[Entity])] -> [Entity]
elifCascade []       = [Pass]
elifCascade [(e,br)] =  [
    Elif e,
    IndentedBlock br
    ]
elifCascade (branch:es) =
    elifCascade [branch] ++ elifCascade es

absVcat :: [Entity] -> TPP.Doc
absVcat [] = text ""
absVcat (e:es) = case e of 
                IndentedBlock body -> vcat [indent (absVcat body)
                                            , absVcat es
                                            ]
                Dictionary body -> vcat ["self.dict = {"
                                        , indent (absVcat body)
                                        , "}"
                                        , absVcat es
                                        ]
                _                  -> vcat [text $ PPP.printTree e
                                        , absVcat es
                                        ]
                                        
emptyConstructor :: [Entity]
emptyConstructor = method (Right Init) [Left Self] [Pass] 

type MethodName = Either String Entity
type FormalParameter = Either Entity (String, Maybe String)

method :: MethodName -> [FormalParameter] -> [Entity] -> [Entity]
method (Left x) args body  = absMethod (AnyMethod $ Ident x) args body 
method (Right e) args  body = absMethod e args body

absMethod :: Entity -> [FormalParameter] -> [Entity] -> [Entity]
absMethod name args body = [ Method $ Function name (formalParams args)
                           , IndentedBlock body
                           ]

formalParams :: [FormalParameter] -> [Entity]
formalParams x = map formalParam x 

formalParam :: FormalParameter -> Entity
formalParam (Right (p, t)) = Argument (Ident p) (argType t)
                       where 
                         argType t = case t of
                                    Just typ -> YesType $ Ident typ
                                    _        -> NoType 
formalParam (Left p) = p

getUserCategories :: CF -> [(Cat, [(Fun, [Cat])])]
getUserCategories cf = [(c,labs) | (c,labs) <- getAbstractSyntax cf , not $ isList c]

getUserTokens :: CF -> [Cat]
getUserTokens cf = [catIdent, catString] ++ (fst $ unzip $ tokenPragmas cf)

filterTerminals :: CF -> [Cat] -> [Cat]
filterTerminals cf ls = [ cat | cat <- ls , not $ cat `elem`  (getUserTokens cf)]

dictionaryName :: Entity
dictionaryName = Id $ (Ident "dict")

dictionaryRef :: Entity
dictionaryRef =  dictionaryName

getParams :: [Cat] -> [(String,String)]
getParams = nameFormalParameters Dma.empty 
-- returns a list of pairs:
--      fst: name of formal parameters
--      snd: formal parameter type (type) 
nameFormalParameters :: Dma.Map Cat Integer -> [Cat] -> [(String,String)]
nameFormalParameters mp [] = []
nameFormalParameters mp (f:fs) = [fparam]
                                ++nameFormalParameters 
                                        (Dma.insertWith (+) f 1 mp) 
                                        fs
    where
      ident = identCat f
      name = ident++"_"++case Dma.lookup f mp of
                        Just x -> show x
                        _      -> ""
      fparam   = (fparName,typename) 
      typename = if isList f
                   then "list"
                   else ident
      fparName = map toLower name

tupleLiteral :: [Entity] -> Entity
tupleLiteral x = Function (mkId "") x 

listSingleton :: Entity -> Entity
listSingleton x = SquareBracketAccess (mkId "") (YesArray x)

emptyList :: Entity
emptyList = listSingleton NothingPython

instVar x = toNames [Self, mkId x]
module BNFC.Backend.Python.Utils where
import BNFC.Backend.Python.PrintPython as PPP
import BNFC.Backend.Python.AbsPython 
import Text.PrettyPrint as TPP

indent = nest 4

importList li = vcat $ map ("import"<+>) li

toNames :: [Entity] -> [Name]
toNames ent = [Name n | n <- ent]

absVcat :: [Entity] -> TPP.Doc
absVcat [] = text ""
absVcat (e:es) = case e of 
                IndentedBlock body -> vcat [indent (absVcat body)
                                            , absVcat es
                                            ]
                Dictionary body -> vcat ["{"
                                        , indent (absVcat body)
                                        , absVcat es
                                        , "}"
                                        ]
                _                  -> vcat [text $ PPP.printTree e
                                        , absVcat es
                                        ]
                                        
emptyConstructor :: [Entity]
emptyConstructor = method (Right Init) [Left Self] [Pass] 


type FormalParameter = Either Entity (String, Maybe String)

method :: Either String MethodName -> [FormalParameter] -> [Entity] -> [Entity]
method (Left x) args body  = absMethod (AnyMethod $ Ident x) args body 
method (Right e) args  body = absMethod e args body

absMethod :: MethodName -> [FormalParameter] -> [Entity] -> [Entity]
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

-- data PythonDefinition = PyMethod String [Cat] | PyClass | PyChildClass String   


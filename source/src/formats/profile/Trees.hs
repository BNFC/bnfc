module Trees where

data Exp =
   EApp  Exp Exp
 | EAbs  Ident Exp
 | EAtom Atom
  deriving (Eq,Ord,Show)

newtype CFTree = CFTree (CFFun,[CFTree]) deriving (Eq, Show)

type CFCat = Ident

newtype Ident = Ident String deriving (Eq, Ord, Show)

-- to build trees: the Atom contains a GF function, Cn | Meta | Vr | Literal
newtype CFFun = CFFun (Atom, Profile) deriving (Eq,Ord,Show) 

type Profile  = [([[Int]],[Int])]

data Atom =
   AC Ident
 | AV Ident
 | AM
 | AS String
 | AI Integer
  deriving (Eq,Ord,Show)

-- printing

class Prt a where
  prt :: a -> String

instance Prt Exp where
  prt e = case e of
    EApp f a -> unwords [prt f, prt1 a]
    EAbs x a -> "\\" ++ prt x ++ " -> " ++ prt a
    EAtom a  -> prt a
   where
     prt1 e = case e of
       EAtom _ -> prt e
       _ -> "(" ++ prt e ++ ")"

instance Prt Atom where
  prt a = case a of
    AC x -> prt x
    AV x -> prt x
    AM   -> "?"
    AS s -> show s ----
    AI i -> show i

instance Prt Ident where
  prt (Ident x) = x

-- printing trees

prCFTree :: CFTree -> String
prCFTree (CFTree (fun, trees)) = prCFFun fun ++ prs trees where
 prs [] = ""
 prs ts = " " ++ unwords (map ps ts)
 ps t@(CFTree (_,[])) = prCFTree t
 ps t = "(" ++ prCFTree t ++ ")"

prCFFun :: CFFun -> String
prCFFun = prCFFun' True ---- False -- print profiles for debug

prCFFun' :: Bool -> CFFun -> String
prCFFun' profs (CFFun (t, p)) = prt t ++ pp p where
    pp p = if (not profs || normal p) then "" else "_" ++ concat (map show p)
    normal p = and [x==y && null b | ((b,x),y) <- zip p (map (:[]) [0..])]

prCFCat :: CFCat -> String
prCFCat c = prt c

mkFunTree :: String -> Profile -> [CFTree] -> CFTree
mkFunTree f p ts = CFTree (CFFun (AC (Ident f),p), ts)

mkAtTree :: Atom -> CFTree
mkAtTree a = CFTree (CFFun (a,[]), [])

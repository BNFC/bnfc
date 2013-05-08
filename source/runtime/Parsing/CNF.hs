{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, FlexibleInstances #-}
module CNF where
-- Tools for grammars in CNF (or Bin).

import qualified Prelude as P
import Data.List (nub,partition)
import Zero
import Control.Applicative
import Data.Traversable
import Control.Monad (join)

trans :: Eq a => [(a,a)] -> [(a,a)]
trans xys = iter [] xys
 where
  iter rel [] =
    rel

  iter rel (xy:xys) | xy `elem` rel =
    iter rel xys

  iter rel (xy@(x,y):xys) =
    iter (xy:rel) (new ++ xys)
   where
    new = [ (a,y) | (a,b) <- rel, b==x ]
       ++ [ (x,b) | (a,b) <- rel, y==a ]

class Token tok where
    -- | For printing
    tokToString :: tok -> String


class (Eq nt, Token token, Eq token, Bounded nt, Enum nt)
   => Grammar nt token | nt -> token where
    -- | All NTs generating a given token
    tokNT :: token -> [nt]
    -- | productions a b = All NTs producing "a b"
    productions :: nt -> nt -> [nt]

    startcats :: [nt]

    allNTs :: [nt]
    allNTs = [minBound .. maxBound]

    merges :: [(nt,nt)]
    merges =
     [ (x,y)
       | x <- allNTs
       , y <- allNTs
       , not (null (x `productions` y))
     ]
     -- (a,b) in lefts,  iff a can be on the left spine of a valid parse tree with b at the top
     -- (a,b) in rights, iff a can be on the right spine of a valid parse tree with b at the top
    lefts :: [(nt,nt)]
    lefts =
      trans [ (a,c)
            | a <- allNTs
            , b <- allNTs
            , c <- a `productions` b
            ]
    rights :: [(nt,nt)]
    rights =
      trans [ (b,c)
            | a <- allNTs
            , b <- allNTs
            , c <- a `productions` b
            ]
        -- (a,b) in leftNbs,  iff a as a left  neighbour of b is a valid reason to keep b alive
        -- (a,b) in rightNbs, iff b as a right neighbour of a is a valid reason to keep a alive
    leftNbs, rightNbs :: [(nt,nt)]
    leftNbs =
      nub
      [ (a,b)
      | (c,b) <- merges
      , (a,c') <- rights
      , c == c'
      ]

    rightNbs =
      nub
      [ (a,b)
      | (a,c) <- merges
      , (b,c') <- lefts
      , c == c'
      ]

    -- a in leftEdge,  iff a should be kept alive on the left-edge of a chart
    -- a in rightEdge, iff a should be kept alive on the right-edge of a chart
    leftEdge, rightEdge :: [nt]
    leftEdge =
      nub
      [ b
      | (a,b) <- merges
      ]

    rightEdge =
      nub
      [ a
      | (a,b) <- merges
      ]


class (Eq nt, Token token, Eq token, Bounded nt, Enum nt)
   => GrammarP nt token | nt -> token where
    -- | All NTs generating a given token
    tokNTP :: Bool -> token -> Pair [nt]
    tokNTP _ = pure . tokNTN
    tokNTN :: token -> [nt]
    -- | productions a b = All NTs producing "a b"
    productionsP :: Bool -> nt -> nt -> Pair [nt]
    productionsP _ x y = pure $ productionsN x y
    productionsN :: nt -> nt -> [nt]


data AST cat token = L cat token
                   | B cat (AST cat token) (AST cat token)
          deriving (Eq)

-- | Linearisation of an AST.
lin (L _ s) = tokToString s
lin (B _ x y) = lin x ++ " " ++ lin y

-- | Category of an AST
cat (L x _) = x
cat (B x _ _) = x


instance (GrammarP nt token, Show nt) => Show (AST nt token) where
  show = sho

sho (L x t)   = show x ++ "=" ++ tokToString t
sho (B x s t) = show x ++ "{" ++ sho s ++ "," ++ sho t ++ "}"


type Set a = [a]


-- Sets form an abelian group
instance AbelianGroup (Set a) where
    zero = []
    (+) = (++)

instance AbelianGroupZ (Set a) where
    isZero = null

-- Sets of ASTs form a ring, lifting the production rules.
instance Grammar nt token => Ring (Set (AST nt token)) where
    xs * ys = [B c x y | x <- xs, y <- ys, c <- productions (cat x) (cat y)]

instance GrammarP nt token => RingP (Set (AST nt token)) where
    mul p xs ys = join <$> sequenceA [map (\z -> B z x y) <$> productionsP p (cat x) (cat y) | x <- xs, y <- ys]




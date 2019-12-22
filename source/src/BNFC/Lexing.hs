{-# LANGUAGE PatternGuards #-}

module BNFC.Lexing
    ( mkLexer, LexType(..), mkRegMultilineComment ) where

import Prelude hiding ((<>))

-- import Control.Arrow ( (&&&) )
import Data.List     ( inits, tails )

import AbsBNF        ( Reg(..)   )
import PrintBNF      ( printTree )  -- for debug printing
import BNFC.CF
import BNFC.Regex    ( simpReg )
import BNFC.Utils    ( unless  )

debugPrint :: Reg -> IO ()
debugPrint = putStrLn . concat . words . printTree

-- Abstract lexer

data LexType = LexComment | LexToken String | LexSymbols

mkLexer :: CF -> [(Reg, LexType)]
mkLexer cf = concat
    -- comments
  [ [ (mkRegSingleLineComment s, LexComment) | s <- snd (comments cf) ]
  , [ (mkRegMultilineComment b e, LexComment) | (b,e) <- fst (comments cf) ]
    -- user tokens
  , [ (reg, LexToken name) | (name, reg) <- tokenPragmas cf]
    -- predefined tokens
  , [ ( regIdent, LexToken "Ident" ) ]
    -- Symbols
  , unless (null $ cfgSymbols cf) [ (foldl1 RAlt (map RSeqs (cfgSymbols cf)), LexSymbols ) ]
    -- Built-ins
  , [ ( regInteger, LexToken "Integer")
    , ( regDouble , LexToken "Double" )
    , ( regString , LexToken "String" )
    , ( regChar   , LexToken "Char"   )
    ]
  ]


(<>) = RSeq
(<|>) = RAlt

-- Bult-in tokens
-- the tests make sure that they correspond to what is in the LBNF reference

-- | Ident regex
-- >>> debugPrint regIdent
-- letter(letter|digit|'_'|'\'')*
regIdent :: Reg
regIdent = RLetter <> RStar (RLetter <|> RDigit <|> RChar '_' <|> RChar '\'')

-- | Integer regex
-- >>> debugPrint regInteger
-- digit+
regInteger :: Reg
regInteger = RPlus RDigit

-- | String regex
-- >>> debugPrint regString
-- '"'(char-["\"\\"]|'\\'["\"\\nt"])*'"'
regString :: Reg
regString = RChar '"'
            <> RStar ((RAny `RMinus` RAlts "\"\\")
                       <|> (RChar '\\' <> RAlts "\"\\nt"))
            <> RChar '"'

-- | Char regex
-- >>> debugPrint regChar
-- '\''(char-["'\\"]|'\\'["'\\nt"])'\''
regChar :: Reg
regChar = RChar '\''
          <> (RMinus RAny (RAlts "'\\") <|> (RChar '\\' <> RAlts "'\\nt"))
          <> RChar '\''

-- | Double regex
-- >>> debugPrint regDouble
-- digit+'.'digit+('e''-'?digit+)?
regDouble :: Reg
regDouble = RPlus RDigit <> RChar '.' <> RPlus RDigit
            <> ROpt (RChar 'e' <> ROpt (RChar '-') <> RPlus RDigit)

-- | Create regex for single line comments
-- >>> debugPrint $ mkRegSingleLineComment "--"
-- {"--"}char*'\n'
mkRegSingleLineComment :: String -> Reg
mkRegSingleLineComment s = RSeqs s <> RStar RAny <> RChar '\n'


-- -- | Create regex for multiline comments.
-- --
-- -- >>> debugPrint $ mkRegMultilineComment "<" ">"
-- -- '<'(char-'>')*'>'
-- --
-- -- >>> debugPrint $ mkRegMultilineComment "<!--" "-->"
-- -- {"<!--"}(char-'-')*'-'((char-'-')+'-')*'-'('-'|(char-["->"])(char-'-')*'-'((char-'-')+'-')*'-')*'>'
-- --
-- mkRegMultilineComment :: String -> String -> Reg
-- mkRegMultilineComment b []       = RSeqs b
-- mkRegMultilineComment b (a:rest) = simpReg $ RSeqs b `RSeq` fromStart
--   where
--   notA                       = RAny `RMinus` RChar a
--   goA                        = RStar notA `RSeq` RChar a
--   (fromStart, _, _)          = foldl f (goA, REps, []) rest
--   -- Build up automaton states Start, A, ...ys..., x, ...
--   f (fromStart, fromA, ys) x = (advance fromStart, advance fromA, x:ys)
--     where
--     advance from = (from `RSeq` RStar idle) `RSeq` RChar x
--     idle         = foldl1 RAlt $ concat
--       -- cannot advance, ...
--       [ [ RChar a              | a /= x, all (a ==) ys            ] -- but can stay
--       , [ RChar a `RSeq` fromA | a /= x, null ys || any (a /=) ys ] -- but can fall back to A
--       , [ (RAny `RMinus` RAlts [x,a]) `RSeq` fromStart            ] -- neither, need to restart
--       ]


-- | Create regex for multiline comments.
--
-- >>> debugPrint $ mkRegMultilineComment "<" ">"
-- '<'(char-'>')*'>'
--
-- >>> debugPrint $ mkRegMultilineComment "/*" "*/"
-- {"/*"}(char-'*')*'*'((char-["*/"])(char-'*')*'*'|'*')*'/'
--
-- >>> debugPrint $ mkRegMultilineComment "<!--" "-->"
-- {"<!--"}(char-'-')*'-'((char-'-')+'-')*'-'((char-["->"])(char-'-')*'-'((char-'-')+'-')*'-'|'-')*'>'
--
mkRegMultilineComment :: String -> String -> Reg
mkRegMultilineComment begin end = simpReg $ joinSteps (RSeqs begin) allSteps
  where

  -- This handles cases beyond comment terminators such as "*/" and "-->".
  -- In the general but unlikely case, a comment terminator may have
  -- non-trivial internal repetitions, like in "ananas".  While lexing
  -- "anananas", we need, after having seen "anana", fall back to state
  -- "ana", to correctly handle the rest "nas" of the input and recognize the
  -- comment terminator.

  -- See the Knuth-Morris-Pratt algorithm of complexity O(n+m) to recognize a
  -- keyword of length m in a text of length n.
  -- (Dragon book second edition section 3.4.5;
  -- Knuth/Morris/Pratt (J. Computing 1977),
  -- "Fast pattern matching on strings").

  -- The basic idea is to construct the regular expression to recognize
  -- a text not containing @end@ but ending in @end@ from this DFA:
  --
  -- * DFA-states: the prefixes of @end@, formally @inits end@,
  --   written a(1..i) for @i <= length end@.
  --
  -- * Primary transitions ("spine") take us from state a(1..i) (called @ys@)
  --   to a(1..i+1) (called @x:ys@), consuming character a(i+1) (called @x@).
  --
  -- * Fallback transitions take us from state a(1..i) (@ys@) to some previous
  --   state a(1..j) with j <= i, consuming character @z@=a(j) (unless j=0).
  --   The main condition for fallbacks is a(i-j+2..i)=a(1..j-1) ("suffix = prefix"),
  --   because then we can append a(j) to our truncated history a(i-j+2..i)
  --   and end up in a(1..j).
  --   The secondary condition is that we are obliged to not fall back further
  --   than we must:  If consuming @z@ can get us to a(1..k) with k > j,
  --   we cannot fall back to a(1..j).
  --
  -- The final @Reg@ transitions along the spine also involve "idling" on a state,
  -- meaning transition sequences bringing us back to the same state.
  -- The list @steps@ will contain the "spine" transitions (a(1..i)->a(1..i+1))
  -- _including_ the idling.  The first entry in the list is the last transition
  -- computed so far.  @allSteps@ is then the complete @steps@ list, which can be
  -- joined by @RSeq@ (function @joinSteps@).
  --
  -- Remark:
  -- Note that the generated regex can be too big for lexers to handle.
  -- For the example @end == "ananas"@, ocamllex uses up ~30.000 of its
  -- 32.767 maximal automaton transitions, which prevents comments
  -- ending in "ananas" to be part of a ocamllex lexer definition in practice.
  -- The Haskell lexer generator Alex is slow as well on this example,
  -- although the produced lexer is unproblematic in the end.
  --
  -- Lexer generators _should_ be able to handle the regex we are producing here
  -- because the DFA has only O(n) states and O(n²) transitions where @n = length end@
  -- is the length of the comment terminator @end@.
  --
  -- It is just an awkward way to generate this DFA via the detour over a regex
  -- which in turn is dictated by the interface of lexer generators.
  -- The size of the regex tree seems to be O(n³)!?
  -- It would be much smaller as DAG (tree with sharing).
  -- Lexer generators often support regex definitions; we could make each entry
  -- in @steps@ a defined regex.  However, it is not clear whether this sharing
  -- is utilized in the regex → NFA construction in the lexer generators.
  joinSteps :: Reg -> [Reg] -> Reg
  joinSteps = foldr (flip RSeq)
  -- Transitions of the spine of the automaton, with last transition first in the list.
  allSteps :: [Reg]
  allSteps = fst $ foldl next ([],[]) end

  -- @next (steps, ys) x@ calculates the next step,
  -- taking us from state @ys@ to state @x:ys@.
  next :: ([Reg],[Char]) -> Char -> ([Reg],[Char])
  next
    ( steps  -- [r(i-1,i), ..., r(0,1)], empty if i == 0
    , ys     -- [a(i),...,a(1)]        , empty if i == 0
    ) x      -- a(i+1)
    = (step : steps, x:ys)
    where

    -- step = r(i,i+1) is the regular expression to go to next state.
    -- We can idle on state a(1..i) and then take the transition to a(1..i+1).
    step :: Reg
    step = RStar idle `RSeq` RChar x
    -- @idle@ presents all the possibilities to stay on the current state
    -- or fall back to a previous state and then again advance to the present state.
    -- We consider first the possibility to fall back to the start state a(1..0),
    -- and then the possibility to fall back to a(1..1), then, to a(1..2), etc.,
    -- until staying on a(1..i).
    -- We are obliged to stay as far advanced as possible, we can only fall
    -- father back if we cannot stay more upfront.
    -- Transitioning to state a(1..j) is possible if
    --   * the next character is not x (a(i+1)),
    --   * the next character is a(j),
    --   * the last j-1 characters we processed, a(i-j+2..j) are a(1..j-1),
    --   * we cannot transition to a(1..j+1), a(1..j+2), ..., a(1..i).
    idle :: Reg
    idle = foldl RAlt toStart $ map snd possibilities
      where
      -- List of possibilities to go back to a previous state upon
      -- the given character and how to return to the present state.
      -- We calculate the possibilities in order of:
      --   * staying on the current state
      --   * falling back one state
      --   * falling back two states
      --   * ...
      --   * falling back to the start.
      -- The reason is that falling back further than necessary is not allowed.
      possibilities :: [(Char,Reg)]
      possibilities = foldl addPoss [] (zip3 ys conds $ inits steps)
      -- Fall back to the beginning and come back to the present state.
      toStart :: Reg
      toStart = joinSteps (RAny `RMinus` RAlts (x : map fst possibilities)) steps
      -- Adding a possiblity on top of the existing ones.
      addPoss :: [(Char,Reg)] -> (Char,Bool,[Reg]) -> [(Char,Reg)]
      addPoss
        poss                -- List of possibilities (a(k),r) of falling back to a(k) and recovering to a(i) via r.
        (z, cond, steps)    -- Investigating possibility to fall back to a(1..j) where cond says this is in principle
                            -- possible if we read @z@, not @x@, and none of the previous possibilities.
                            -- @steps@ brings us back to the current state (after falling back).
        | cond, z `notElem` exclude = (z, joinSteps (RChar z) steps) : poss
        | otherwise = poss
        where
        -- To fall back with @z@, we need to exclude the possibility of
        -- advancing (via character @x@) and falling back less.
        exclude :: [Char]
        exclude = x : map fst poss
      -- Conditions of whether a fallback is in principle possible,
      -- starting with the state we have been in previously, ending in the first state.
      -- If we are in state a(1..i), the possibility of falling back to a(1..j)
      -- is constrained on a(1..j-1) = a(i-j+2..i).
      conds :: [Bool]
      conds = zipWith (==) (tail $ reverse $ inits ys) (tail $ tails ys)

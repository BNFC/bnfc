{-|
  Module      : BNFC.RegexMinus
  Description : Converts away subtraction in regexes
  License     : Public Domain

  This module is intended to be imported qualified to use in lexer backends
  which wish to support general subtraction.

  Inspired by David J. Sankel's implementation.

  Many ideas taken from:

    - <http://home.chello.no/~mgrsby/sgmlintr/file0003.htm>
    - <http://home.chello.no/~mgrsby/sgmlintr/file0004.htm>
    - <http://home.chello.no/~mgrsby/sgmlintr/file0005.htm>
    - <https://arxiv.org/pdf/1502.03573>

  (accessible via the Wayback Machine).

  = EXPLANATION

  First, we define:

  - λ = the empty string "";
  - φ = the empty language [];
  - δR (for some regex R) = if λ∊R then λ else φ.
    For example, δ(a*b*) = λ; δ(ab*) = φ;
  - The derivative Dc(R) (for some character c and regex R) = a regex S such
    that S matches exactly the strings that are in R and start with @c@, but
    without the first character @c@.
    For example, Da((ab|cd)*) = b(ab|cd)*; Da(a*) = a*; Da(λ) = φ.

  Also note that:

  - λR = Rλ = R;
  - φR = Rφ = φ;
  - R|φ = R;
  - R-φ = R;
  - φ-R = φ;
  - Dc(R-S)=DcR-DcS.

  The main observation: if R contains strings that start with
  @a@, @b@, ..., @z@, then

                     R = a(DaR)|b(DbR)|...|z(DzR)|δR.

  Our strategy is to derive the subtraction until we arrive at a new subtraction
  that we have seen before, or eliminate the subtrahend.

  We obtain an FSA with states representing different subtractions and
  transitions being the characters we derive on. We add a fictitious final state
  and add transitions to it:
  - from all nullable states (where δR=λ) - a spontaneous transition λ;
  - from all states where a derivation on a character "c" eliminates the
    subtraction (Dc(R-S) = T) - a transition "cT".

  The obtained FSA is converted to a regex using state elimination
  (<https://arxiv.org/pdf/1502.03573>, section 3.2).

  Let's consider an example: we have to convert (ab|ac)*-(ac)* to an equivalent
  regex without the subtraction operator.

  We name the initial state (R).

  > Da(R)  = (b|c)(ab|ac)* - c(ac)* = (new state S1)
  > Db(S1) = (ab|ac)*  (subtraction eliminated)
  > Dc(S1) = (ab|ac)* - (ac)* = (loop to R)

  Since δR=δS1=φ, we have no spontaneous transitions to the final state.
  The FSA:

     (---) ----a----> (----)                  (-----)
  -->( R )            ( S1 ) ---b(ab|ac)*---> ( Fin )
     (___) <----c---- (____)                  (_____)

  We eliminate S1 by adding an R->R transition "aφ*c" = "ac" and an R->Fin
  transition "aφ*b(ab|ac)*" = "ab(ab|ac)*":

  -->(---) ---ab(ab|ac)*-->(-----)
     ( R )                 ( Fin )
   +-(___)<-+              (_____)
   |        |
   +---ac --+

  We eliminate R and obtain (ac)*ab(ab|ac)*.

  This implementation assigns a unique ID to every regex and uses the IDs to
  compare states.
-}

module BNFC.RegexMinus
  (
    -- * The 'SimpleRegex' type and its smart constructors
    SimpleRegex(..)
  , charset
  , string

    -- * 'SimpleRegex' conversions
  , toSimpleRegex
  , regexToString

    -- * 'SimpleRegex' transformations
  , removeMinuses
  ) where

-- Data types
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

-- Interaction with the Reg type
import qualified BNFC.Abs as Abs

-- | Describes a regular expression.
-- Purposefully minimalistic for simpler processing.
data SimpleRegex a
  = Term a  -- ^ Matches one character
  | Lambda  -- ^ This is the 0-length string
  | Phi     -- ^ Recognizes no strings
  | Rep (SimpleRegex a)  -- ^ Kleene Star (*)
  | Or (SimpleRegex a) (SimpleRegex a)   -- ^ Or (|)
  | Sub (SimpleRegex a) (SimpleRegex a)  -- ^ Regex Subtraction (-)
  | Seq (SimpleRegex a) (SimpleRegex a)  -- ^ Sequence (ab)
  deriving (Eq, Ord, Show)

-- | Converts from richer canonical 'Abs.Reg's
-- to the 'SimpleRegex' representation.
toSimpleRegex :: Ord a =>
     (Char -> SimpleRegex a)  -- ^ converts a single character.
  -> SimpleRegex a  -- ^ all Unicode characters.
  -> SimpleRegex a  -- ^ all digits.
  -> SimpleRegex a  -- ^ all isolatin1 letters.
  -> SimpleRegex a  -- ^ all uppercase isolatin1 letters.
  -> SimpleRegex a  -- ^ all lowercase isolatin1 letters.
  -> Abs.Reg        -- ^ the 'Abs.Reg' to simplify into a 'SimpleRegex'.
  -> SimpleRegex a
toSimpleRegex fromChar any digit letter upper lower = helper
  where
    helper = \case
      Abs.RAlt l r   -> Or (helper l) (helper r)
      Abs.RMinus l r -> Sub (helper l) (helper r)
      Abs.RSeq l r   -> Seq (helper l) (helper r)
      Abs.RStar reg  -> Rep (helper reg)
      Abs.RPlus reg  -> let sreg = helper reg in sreg `Seq` Rep sreg
      Abs.ROpt reg   -> Lambda `Or` helper reg
      Abs.REps       -> Lambda
      Abs.RChar ch   -> fromChar ch
      Abs.RAlts s    -> foldr Or Phi $ map fromChar s
      Abs.RSeqs s    -> foldr Seq Lambda $ map fromChar s
      Abs.RDigit     -> digit
      Abs.RLetter    -> letter
      Abs.RUpper     -> upper
      Abs.RLower     -> lower
      Abs.RAny       -> any

-- | Returns a 'SimpleRegex' that matches any one of the items in the list.
charset :: Ord a => [a] -> SimpleRegex a
charset = \case
  []    -> Phi
  chars -> foldr1 Or $ map Term chars

-- | Returns a 'SimpleRegex' that matches the exact sequence of items as in the
-- list.
string :: Ord a => [a] -> SimpleRegex a
string = \case
  []    -> Lambda
  chars -> foldr1 Seq $ map Term chars

-- | Used in place of actual regexes. Makes equality checking doable in /O(1)/.
newtype RegexID = RegexID Int
  deriving (Ord, Eq, Show)

-- | Extracts the @Int@ representation of the t'RegexID'.
-- Used with @IntMap@s and @IntSet@s.
regexID2Int :: RegexID -> Int
regexID2Int (RegexID i) = i

-- | A more convenient representation of regex trees.
-- The empty language is represented as @v'RegexNodeOr' IntSet.empty@.
data RegexNode a
  = RegexNodeTerm !a                -- ^ Matches the single element @a@.
  | RegexNodeEmpty                  -- ^ Matches the empty string.
  | RegexNodeOr !IntSet             -- ^ Union of regexes with IDs in the set.
  | RegexNodeSeq !RegexID !RegexID  -- ^ Sequence of regexes with given IDs.
  | RegexNodeStar !RegexID          -- ^ Kleene star of the regex with given ID.
  deriving (Ord, Eq)

-- | 'SimpleRegex' with precomputed possible starting characters nullability
data AnnotatedRegexNode a = AnnotatedRegexNode
  { regexID     :: !RegexID        -- ^ The ID.
  , regexNode   :: !(RegexNode a)  -- ^ The regex tree node.
  , regexStarts :: !(Set a)
    -- ^ all characters/elements that start a string matching the regex.
  , regexContainsEmpty :: !Bool
    -- ^ Does the empty string match this regex?
  }

-- | Bidirectional mapping:
-- t'RegexID' <-> the t'AnnotatedRegexNode' with precomputed values
data RegexTrees a = RegexTrees
  { tree2id :: (Map (RegexNode a) (AnnotatedRegexNode a))
    -- ^ Maps the tree node to its ID.
  , id2tree :: (IntMap (AnnotatedRegexNode a))
    -- ^ Maps the node ID to the tree node.
  }

-- | Returns the empty t'RegexTrees' data structure.
emptyRegexTrees :: Ord a => RegexTrees a
emptyRegexTrees = RegexTrees
  { tree2id = Map.empty
  , id2tree = IntMap.empty
  }

-- | Returns the information about the regex tree node by its ID.
-- This throws on lookup failure because if the algorithm is correct then
-- no lookup should fail.
getByID :: RegexID -> RegexTrees a -> AnnotatedRegexNode a
getByID (RegexID regID) mp = case regID `IntMap.lookup` id2tree mp of
  Nothing   -> error "RegexID not found in map"
  Just node -> node

-- | Insert a new node into the mapping.
-- Assumes that such an (ID, node) combination has not been inserted before.
-- Constructing the t'AnnotatedRegexNode' is the user's responsibility for
-- performance reasons.
insertNode :: Ord a =>
     AnnotatedRegexNode a
  -> RegexTrees a
  -> RegexTrees a
insertNode annot mp = RegexTrees
  { tree2id = Map.insert (regexNode annot) annot $ tree2id mp
  , id2tree = IntMap.insert (regexID2Int $ regexID annot) annot $ id2tree mp
  }

-- | Looks up the regex node in the map.
getByNode :: Ord a =>
     RegexNode a
  -> RegexTrees a
  -> Maybe (AnnotatedRegexNode a)
getByNode node mp = node `Map.lookup` tree2id mp

-- | Creates a new node or returns an existing one if it is exactly the same.
getOrNewID :: Ord a =>
     RegexNode a   -- ^ The contents of the regex node.
  -> Set a         -- ^ Start characters the regex accepts.
  -> Bool          -- ^ Does the regex accept the empty string?
  -> RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewID node regexIDStarts regexIDContainsEmpty mp =
  case getByNode node mp of
    Just annot -> (mp, annot)
    Nothing    -> let
        newID = IntMap.size $ id2tree mp
        annot = AnnotatedRegexNode
          { regexID            = RegexID newID
          , regexNode          = node
          , regexStarts        = regexIDStarts
          , regexContainsEmpty = regexIDContainsEmpty}
      in (insertNode annot mp, annot)

-- | Returns the empty string regex (creates one if none exists yet).
getOrNewLambda :: Ord a =>
     RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewLambda = getOrNewID RegexNodeEmpty Set.empty True

-- | Returns the empty language regex (creates one if none exists yet).
getOrNewPhi :: Ord a =>
     RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewPhi = getOrNewID (RegexNodeOr IntSet.empty) Set.empty False

-- | Returns a new regex that is equivalent to the union of the given regexes.
-- Unites nested v'RegexNodeOr's; if only one regex is given,
-- returns it unmodified. If an exact copy of the new v'RegexNodeOr' has already
-- been created, returns that one.
getOrNewOr :: Ord a =>
     [AnnotatedRegexNode a]  -- ^ Alternatives to unite.
  -> RegexTrees a            -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewOr anns mp = let
    uniqAnns = IntMap.fromList $ map (\ a -> (regexID2Int $ regexID a, a))
      $ concatMap listAnns anns
  in case IntMap.toList uniqAnns of
    [(_, ann)] -> (mp, ann)
    _ -> getOrNewID (RegexNodeOr $ IntMap.keysSet uniqAnns)
      (Set.unions $ map regexStarts $ IntMap.elems uniqAnns)
      (any regexContainsEmpty $ IntMap.elems uniqAnns) mp
  where
    -- listAnns :: AnnotatedRegexNode a -> [AnnotatedRegexNode a]
    listAnns ann =
      case regexNode $ getByID (regexID ann) mp of
        RegexNodeOr idset -> map (flip getByID mp . RegexID)
          $ IntSet.toList idset
        _                 -> [ann]

-- | Returns a new regex that is a sequence of the given two.
-- Normalizes such that the left regex is not a v'RegexNodeSeq' itself
-- (maintains right-associativity). This is done so that removing one term from
-- the left is fast.
-- Removes v'RegexNodeEmpty' regexes; returns an empty language if the left or
-- the right argument is an empty language.
-- If an exact copy of the new v'RegexNodeSeq' has already been created,
-- returns that one.
getOrNewSeq :: Ord a =>
     AnnotatedRegexNode a  -- ^ The left node.
  -> AnnotatedRegexNode a  -- ^ The right node.
  -> RegexTrees a          -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewSeq left right mp = let rightNode = regexNode right in
    if isPhi rightNode
    then (mp, right)
    else case rightNode of
      RegexNodeEmpty -> (mp, left)
      _              -> helper left right mp
  where
    -- | Does not check @right@.
    helper left right mp = let
        leftID   = regexID left
        leftNode = regexNode left
      in
        if isPhi leftNode
        then (mp, left)
        else case leftNode of
          RegexNodeSeq llID lrID -> let
              ll = getByID llID mp
              lr = getByID lrID mp
              (mp1, mid) = helper lr right mp
            in helper ll mid mp1
          RegexNodeEmpty         -> (mp, right)
          _                      -> getOrNewID
            (RegexNodeSeq leftID (regexID right))
            (if regexContainsEmpty left
              then Set.union (regexStarts left) (regexStarts right)
              else regexStarts left)
            (regexContainsEmpty left && regexContainsEmpty right) mp
    -- | Is the regex an empty language?
    isPhi = \case
      RegexNodeOr idset -> IntSet.null idset
      _                 -> False

-- | Returns a new or existing regex matching a single term (e.g. @Char@).
getOrNewTerm :: Ord a =>
     a             -- ^ The singular regex term (@Char@, @Int8@...).
  -> RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewTerm ch = getOrNewID (RegexNodeTerm ch) (Set.singleton ch) False

-- | Returns a new or existing regex matching zero or more strings that the
-- argument matches (Kleene star).
getOrNewStar :: Ord a =>
     AnnotatedRegexNode a  -- ^ The regex to put under a Kleene star.
  -> RegexTrees a          -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
getOrNewStar node =
  getOrNewID (RegexNodeStar (regexID node)) (regexStarts node) True

-- | Converts the simple regex into an internal representation with precomputed
-- starts and nullability.
-- Converts away subtractions.
makeAnnotated :: Ord a =>
     SimpleRegex a
     -- ^ The regular expression to convert (could contain subtractions).
  -> RegexTrees a  -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
makeAnnotated reg mp = case reg of
  Term a -> getOrNewTerm a mp
  Lambda -> getOrNewLambda mp
  Phi    -> getOrNewPhi mp
  Rep r  -> let (newmp, rann) = makeAnnotated r mp in getOrNewStar rann newmp
  Or a b -> let
      rs = flattenOr a ++ flattenOr b
      (newmp, anns) =
        foldr (\ r (mp, anns) ->
          let (newmp, ann) = makeAnnotated r mp in (newmp, ann : anns))
        (mp, []) rs
    in getOrNewOr anns newmp
  Sub a b -> let
      (mp1, nodeA) = makeAnnotated a mp
      (mp2, nodeB) = makeAnnotated b mp1
    in convertSub nodeA nodeB mp2
  Seq a b -> let
      (a', b')     = reorderSeq a b
      (mp1, nodeA) = makeAnnotated a' mp
      (mp2, nodeB) = makeAnnotated b' mp1
    in getOrNewSeq nodeA nodeB mp2
  where
    flattenOr = \case
      Or a b -> flattenOr a ++ flattenOr b
      Phi    -> []
      other  -> [other]
    -- | Maintains right-associativity of v'Seq'.
    reorderSeq ::
         SimpleRegex a
      -> SimpleRegex a
      -> (SimpleRegex a, SimpleRegex a)
    reorderSeq a b = case a of
      Seq l r -> reorderSeq l (Seq r b)
      _       -> (a, b)

-- | Computes the derivative:
-- Da(a)   = λ
-- Da(b)   = φ
-- Da(λ)   = φ
-- Da(φ)   = φ
-- Da(AB)  = DaA|(δA)DaB
-- Da(A|B) = DaA|DaB
-- Da(A*)  = (DaA)A*
derive :: Ord a =>
     a                     -- ^ The starting character.
  -> AnnotatedRegexNode a  -- ^ The regex to derive.
  -> RegexTrees a          -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
derive ch reg mp = case regNode of
    RegexNodeTerm a ->
      if ch == a
      then getOrNewLambda mp
      else getOrNewPhi mp
    RegexNodeEmpty   -> getOrNewPhi mp
    RegexNodeOr regs -> let
        (newmp, derivs) = IntSet.foldr (\ regID (mp, derivs) ->
          let (mpnew, deriv) = derive ch (getByID (RegexID regID) mp) mp
          in (mpnew, deriv:derivs)) (mp, []) regs
      in getOrNewOr derivs newmp
    RegexNodeSeq leftID rightID -> let
        left               = getByID leftID mp
        right              = getByID rightID mp
        (mp1, derivLeft)   = derive ch left mp
        (mp2, onlyLeftRes) = getOrNewSeq derivLeft right mp1
      in
        if regexContainsEmpty left
        then
          let (mp3, derivRight) = derive ch right mp2
          in getOrNewOr [onlyLeftRes, derivRight] mp3
        else (mp2, onlyLeftRes)
    RegexNodeStar aID -> let
        a             = getByID aID mp
        (mp1, derivA) = derive ch a mp
      in getOrNewSeq derivA reg mp1
  where
    regNode = regexNode reg

-- | Produces an equivalent regex without any v'Sub's
removeMinuses :: Ord a => SimpleRegex a -> SimpleRegex a
removeMinuses reg = convertToSimpleRegex (regexID annot) mp
  where
    (mp, annot) = makeAnnotated reg emptyRegexTrees

-- | An internal record used by @'convertSub'::makeFSA@.
data ConversionState a = ConversionState
  { conv_trees    :: RegexTrees a  -- ^ The current regex collection.
  , conv_fsa      :: FSA a
    -- ^ The finite state automaton that is under construction.
  , conv_vertices :: Map (RegexID, RegexID) Int
    -- ^ Translates subtractions (pairs of minuend and subtrahend)
    -- into FSA nodes.
  , conv_tovisit  :: Set (RegexID, RegexID)
    -- ^ Unvisited FSA nodes (to be visited).
  }

-- | Converts (A-B) into an equivalent regex without subtraction (A and B do not
-- contain subtraction already).
convertSub :: Ord a =>
     AnnotatedRegexNode a  -- ^ The minuend.
  -> AnnotatedRegexNode a  -- ^ The subtrahend.
  -> RegexTrees a          -- ^ The current regex collection.
  -> (RegexTrees a, AnnotatedRegexNode a)
convertSub a b mp =
  -- We create an FSA where state 0 represents a - b (the starting state)
  -- and state 1 is the only accepting state. State 1 does not correspond to a
  -- subtraction expression.
  let
    initstate = (regexID a, regexID b)
    initfsa   = fst $ fsaAddVertex $ fst $ fsaAddVertex fsaEmpty
    ConversionState
      { conv_fsa   = fullfsa
      , conv_trees = mp1
      } = makeFSA ConversionState
        { conv_fsa      = initfsa
        , conv_tovisit  = Set.singleton initstate
        , conv_vertices = Map.singleton initstate startingvertex
        , conv_trees    = mp
        }
    -- Eliminate all created states, leave the 2 original.
    (mp2, reducedfsa) = fullReduceFSA mp1 fullfsa
    Just starttrans = IntMap.lookup startingvertex $ fsa_transitions reducedfsa
  -- The answer is |(0)->(0)|* |(0)->(1)|.
  in case IntMap.lookup finalvertex starttrans of
    Nothing -> getOrNewPhi mp  -- the initial mp is enough
    Just regToFinal -> case IntMap.lookup startingvertex starttrans of
      Nothing  -> (mp2, regToFinal)
      Just reg ->
        let (mp3, repeatstart) = getOrNewStar reg mp2
        in getOrNewSeq repeatstart regToFinal mp3
  where
    startingvertex = 0 :: Int
    finalvertex    = 1 :: Int
    makeFSA convstate_start@ConversionState
        { conv_trees    = mp
        , conv_fsa      = fsa
        , conv_vertices = vertices
        , conv_tovisit  = tovisit_start
        }
      | Set.null tovisit_start = convstate_start
      | otherwise = let
          (mp1, lambda) = getOrNewLambda mp
          ((minuendID, subtrID), tovisit_popped) =
            Set.deleteFindMin tovisit_start
          minuend = getByID minuendID mp1
          subtr = getByID subtrID mp1
          Just thisvertex = Map.lookup (minuendID, subtrID) vertices
          uniqLeftStarts = regexStarts minuend
            `Set.difference` regexStarts subtr
          sharedStarts = regexStarts minuend
            `Set.intersection` regexStarts subtr
          resDelta = regexContainsEmpty minuend
            && not (regexContainsEmpty subtr)
          baseResList = if resDelta then [lambda] else []

          -- derive over terms not subtracted from 'minuend'
          (mp2, immediateResList) = foldr (\ startch (mp_, reslist) -> let
              (mp', deriv) = derive startch minuend mp_
              (mp'', term) = getOrNewTerm startch mp'
              (mp''', seq) = getOrNewSeq term deriv mp''
            in (mp''', seq : reslist)) (mp1, baseResList) uniqLeftStarts

          -- add transition to final state if needed
          (mp3, fsa1) = case immediateResList of
            [] -> (mp2, fsa)
            _  ->
              let (mp', trans) = getOrNewOr immediateResList mp2
              in fsaAddTransition thisvertex finalvertex trans mp' fsa

          -- derive over terms that are subtracted
        in makeFSA $ foldr (\ startch state_ ->
            let
              (mp', derivMinuend) = derive startch minuend $ conv_trees state_
              (mp'', derivSubtr) = derive startch subtr mp'
              newstate = (regexID derivMinuend, regexID derivSubtr)
              (verts', fsa', tovisit', destIndex) = destination newstate
                  (conv_vertices state_) (conv_fsa state_) (conv_tovisit state_)
              (mp''', term) = getOrNewTerm startch mp''
              (mp'''', fsa'') =
                fsaAddTransition thisvertex destIndex term mp''' fsa'
            in ConversionState
              { conv_vertices = verts'
              , conv_trees    = mp''''
              , conv_fsa      = fsa''
              , conv_tovisit  = tovisit'
              }
          ) ConversionState
          { conv_trees    = mp3
          , conv_fsa      = fsa1
          , conv_vertices = vertices
          , conv_tovisit  = tovisit_popped
          } sharedStarts
        where
          destination newstate curVerts curFsa curToVisit =
            case Map.lookup newstate curVerts of
              Nothing -> let
                  (fsa', index) = fsaAddVertex curFsa
                in
                  ( Map.insert newstate index curVerts
                  , fsa'
                  , Set.insert newstate curToVisit
                  , index
                  )
              Just index -> (curVerts, curFsa, curToVisit, index)

    -- | Reduces all new states, leaving the 2 original ones.
    fullReduceFSA mp fsa
      | IntMap.size (fsa_transitions fsa) > 2 =
        uncurry fullReduceFSA $ fsaEliminate mp fsa
      | otherwise = (mp, fsa)

-- | The Finite State Automaton. Represented with an adjacency list of
-- transitions and a (maintained manually) adjacency list of back-transitions.
data FSA a = FSA
  { fsa_transitions :: IntMap (IntMap (AnnotatedRegexNode a))
    -- ^ Map: vertex -> @IntMap.fromList [(destinationNode, transitionRegex)]@.
  , fsa_revEdges    :: IntMap (IntSet)
    -- ^ Map: vertex -> the set of all vertices that have transitions to this
    --                  vertex, __EXCLUDING__ itself.
  }

-- | Creates an empty FSA.
fsaEmpty :: FSA a
fsaEmpty = FSA
  { fsa_transitions = IntMap.empty
  , fsa_revEdges    = IntMap.empty
  }

-- | Creates a new vertex with the next unoccupied index. Indices are numbered
-- starting from 0.
fsaAddVertex ::
     FSA a         -- ^ The finite state automaton to modify.
  -> (FSA a, Int)  -- ^ (The new automaton, the new vertex's index).
fsaAddVertex FSA
  { fsa_transitions = trans
  , fsa_revEdges    = rev
  } = (FSA
    { fsa_transitions = IntMap.insert n IntMap.empty trans
    , fsa_revEdges    = IntMap.insert n IntSet.empty rev
    }, n)
  where
    n = IntMap.size trans

-- | Creates a new transition with the specified regex or merges the regex with
-- an existing transition.
fsaAddTransition :: Ord a =>
     Int                   -- ^ Transition source.
  -> Int                   -- ^ Transition destination.
  -> AnnotatedRegexNode a  -- ^ The transition regex.
  -> RegexTrees a          -- ^ The current regex collection.
  -> FSA a                 -- ^ The FSA to modify.
  -> (RegexTrees a, FSA a)
    -- ^ (The updated regex collection, the modified automaton).
fsaAddTransition from to how mp (FSA
  { fsa_transitions = trans
  , fsa_revEdges    = rev
  }) = let
    Just transFrom = IntMap.lookup from trans
    (mp', trans') = case IntMap.lookup to transFrom of
      Nothing -> (mp, IntMap.update (Just . IntMap.insert to how) from trans)
      Just prevEdge ->
        let (mp1, mergedEdge) = getOrNewOr [prevEdge, how] mp
        in (mp1, IntMap.update (Just . IntMap.insert to mergedEdge) from trans)
    in
      ( mp'
      , FSA
        { fsa_transitions = trans'
        , fsa_revEdges =
            if from /= to
            then IntMap.update (Just . IntSet.insert from) to rev
            else rev
        }
      )

-- | Removes the node with the largest index (last added) from the automaton.
-- Also erases all transitions to and from it.
fsaPop :: Ord a =>
     FSA a  -- ^ The FSA to modify.
  -> (FSA a, Int, IntMap (AnnotatedRegexNode a), IntSet)
    -- ^
    -- ( The modified automaton
    -- , the removed index
    -- , all transitions from the removed vertex (including the one to itself)
    -- , all vertices that used to have transitions to the vertex (excluding
    --   itself)
    -- ).
fsaPop FSA
  { fsa_transitions = trans
  , fsa_revEdges    = rev
  } =
  ( FSA
    { fsa_transitions = IntMap.map (IntMap.delete index) trans'
    , fsa_revEdges    = IntMap.map (IntSet.delete index) rev'
    }
  , index
  , mytrans
  , myRevEdges
  )
  where
    ((index, mytrans),    trans') = IntMap.deleteFindMax trans
    ((_,     myRevEdges), rev') = IntMap.deleteFindMax rev

-- | Eliminates the node with the largest index (last added) from the automaton,
-- but updates other transitions to produce an equivalent FSA.
--
-- If the deleted node is (n), then for all nodes (u) that have transitions to
-- (n) and for all nodes (v) that have transitions from (n), we add the
-- transition |(u)->(n)| |(n)->(n)|* |(n)->(v)|.
fsaEliminate :: Ord a =>
     RegexTrees a           -- ^ The current regex collection.
  -> FSA a                  -- ^ The FSA to modify.
  -> (RegexTrees a, FSA a)
    -- ^ (The updated regex collection, the reduced automaton).
fsaEliminate mp fsa =
  foldr (\ (u, utrans, v, vtrans) (mp_, fsa_) ->
      let (mp', merged) = mergeTrans utrans vtrans mp_
      in fsaAddTransition u v merged mp' fsa_)
    (mp, fsaPopped)
    [ let
        Just fromU  = IntMap.lookup u (fsa_transitions fsa)
        Just utrans = IntMap.lookup index fromU
      in (u, utrans, v, vtrans)
    | u <- IntSet.toList myrev, (v, vtrans) <- IntMap.toList transToOthers
    ]
  where
    (fsaPopped, index, mytrans, myrev) = fsaPop fsa
    (transToOthers, mergeTrans) = case index `IntMap.lookup` mytrans of
      Nothing -> (mytrans, getOrNewSeq)
      Just r  ->
        ( IntMap.delete index mytrans
        , (\ from to mp ->
          let
            (mp', star)  = getOrNewStar r mp
            (mp'', seqr) = getOrNewSeq star to mp'
          in getOrNewSeq from seqr mp'')
        )

-- | Converts an internal 'RegexNode' (represented with a t'RegexID')
-- into a 'SimpleRegex'.
convertToSimpleRegex :: Ord a =>
     RegexID       -- ^ The regex node to convert.
  -> RegexTrees a  -- ^ The current regex collection.
  -> SimpleRegex a
convertToSimpleRegex regID mp = case node of
    RegexNodeEmpty  -> Lambda
    RegexNodeTerm a -> Term a
    RegexNodeSeq leftID rightID ->
      convertToSimpleRegex leftID mp `Seq` convertToSimpleRegex rightID mp
    RegexNodeOr idset ->
      if IntSet.null idset
      then Phi
      else foldr1 Or
        (map (flip convertToSimpleRegex mp . RegexID)
        $ IntSet.toList idset)
    RegexNodeStar rID -> Rep (convertToSimpleRegex rID mp)
  where
    reg  = getByID regID mp
    node = regexNode reg

-- | Visualizes the regex, showing the empty string as () and the empty language
-- as []. Uses parentheses to resolve precedence.
regexToString :: SimpleRegex Char -> String
regexToString = \case
  Term ch -> [ch]
  Lambda  -> ""
  Phi     -> "[]"
  Rep reg -> helper 5 reg ++ "*"
  Or l r  -> helper 2 l ++ "|" ++ helper 2 r
  Sub l r -> helper 1 l ++ "-" ++ helper 2 r
  Seq l r -> helper 3 l ++ helper 3 r
  where
    helper :: Int -> SimpleRegex Char -> String
    helper prec reg
      | prec <= precedence reg = repr
      | otherwise = "(" ++ repr ++ ")"
      where repr = regexToString reg
    precedence = \case
      Term  _ -> 5
      Lambda  -> 0
      Phi     -> 5
      Rep _   -> 4  -- Rep 5
      Or  _ _ -> 2  -- Or 2 2
      Sub _ _ -> 1  -- Sub 1 2
      Seq _ _ -> 3  -- Seq 3 3

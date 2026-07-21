{-# LANGUAGE QuasiQuotes #-}
{-|
  Module      : BNFC.Backend.CPPVar.AbsynGen
  Description : Abstract syntax node classes generator.

  Abstract syntax node classes generator.
-}

module BNFC.Backend.CPPVar.AbsynGen
  (
    -- * The entrypoint
    makeAbsyn
  , GeneratedAbsyn(..)
  , ListItemStorage(..)

    -- * File naming
  , absynHppFilename
  , absynCppFilename

    -- * Utility
  , tokenStorageName
  ) where

-- Language imports
import Prelude hiding ((<>))
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Array as Array
import Data.Array (Array, (!))
import Data.List (intercalate, sort)
import qualified Data.Foldable as Foldable
import qualified Data.Either as Either
import Data.String.QQ (s)

import Text.PrettyPrint (Doc, text, ($+$), empty, nest, (<>), punctuate, comma)

-- BNFC imports
import qualified BNFC.CF as CF
import qualified BNFC.Options as Options
import BNFC.Backend.CPPVar.CPPUtil

-- | The name of the header file defining abstract syntax classes.
absynHppFilename :: String
absynHppFilename = "Absyn.hpp"

-- | The name of the source file implementing abstract syntax classes.
absynCppFilename :: String
absynCppFilename = "Absyn.cpp"

-- | Generates the abstract syntax node classes.
makeAbsyn ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> [CF.Literal]           -- ^ Used built-in tokens.
  -> [CF.Pragma]            -- ^ User-defined pragmas (contain custom tokens).
  -> (String -> Bool)       -- ^ Checks if a token is positional.
  -> [CF.Cat]               -- ^ Parser entrypoints.
  -> MergedGroupedRules
    -- ^ Rule labels in the grammar description,
    -- grouped by the grammar category.
  -> GeneratedAbsyn
makeAbsyn opts literals pragmas isPosToken entrypts mergedGroupedRules
  = GeneratedAbsyn
    { absynCode = CPPHeaderSourcePair
      { cppHeaderText = hpp
      , cppSourceText = cpp
      }
    , absynListItemStorage = listStorage
    }
  where
    utils@AstUtils
      { ast_nsutils = NamespaceUtils
        { nsutils_wrap = maybeNamespace }
      } = newAstUtils opts isPosToken
    StructWithReflection
      { structWithReflection_struct = hppTokenStructs
      , structWithReflection_reflection = hppTokenRefl
      } = headerTokens utils literals pragmas
    (classOrder, listStorage) =
      decideClassDeclarations mergedGroupedRules $ Options.listItemStorage opts
    AbsynNodeCode
      { absynNodeCode_declaration = hppClassDecls
      , absynNodeCode_reflection = hppClassRefl
      , absynNodeCode_implementation = cppRules
      } = defineAllClasses utils listStorage classOrder
    hppFunctions = declareFunctions utils pragmas
    cppFunctions = translateFunctions utils pragmas
    hppMain      = hppTokenStructs $++$ hppClassDecls $++$ hppFunctions
    entrypointRefl = linesToText
      [ "ENTRYPOINT(" ++ catNameNoCoerc cat ++ ");"
      | cat <- removePrecedenceFromCats entrypts
      ]
    hppRefl = reflectionTemplates utils $++$ hppTokenRefl $++$ hppClassRefl
      $++$ entrypointRefl $++$ reflectionUndefs
    hpp = headerHead $++$ maybeNamespace
      (hppMain $++$ wrapNamespace "reflection" hppRefl $++$ funcLocationOf)
    cpp = text ("#include \"" ++ absynHppFilename ++ "\"")
      $++$ maybeNamespace
        (clonePtrImpl $++$ implTokens utils literals pragmas
        $++$ cppRules $++$ cppFunctions)

-- | Code and the decision about list item storage. Returned from 'makeAbsyn'.
data GeneratedAbsyn = GeneratedAbsyn
  { absynCode            :: !CPPHeaderSourcePair  -- ^ Generated code.
  , absynListItemStorage :: !ListItemStorage      -- ^ How items are stored.
  }

-- | How list classes are defined.
data ListItemStorage
  = StoreByValue    -- ^ @std::deque@ of @ItemClass@.
  | StoreByPointer  -- ^ @std::deque@ of @std::unique_ptr@ of @ItemClass@.

------------------------------------------------------------------------
-- * Utility.
------------------------------------------------------------------------

-- | Formats field initializers.
ctorInitializers ::
     [String]  -- ^ Initializer statements.
  -> Doc
ctorInitializers = nest 4 . \case
  []           -> empty
  first : rest -> foldr ($+$) empty
    $ text (": " ++ first) : map (text . (", " ++)) rest

-- | A collection of functions and values for location tracking and referencing
-- the target namespace.
data AstUtils = AstUtils
  {
    -- | The way we track locations.
    astLoc_locKind :: LocationKind

    -- | Adds the correct parameter to the start of a function parameter list
    -- (if needed).
  , astLoc_maybePrependParam ::
         String    -- What name to give to the parameter.
      -> [String]  -- Parameter list.
      -> [String]

    -- | The correct comma-terminated parameter to prepend to a nonempty
    -- parameter list, or an empty string.
  , astLoc_maybeParam ::
         String  -- What name to give to the parameter.
      -> String

    -- | If needed, prepends a simple copy-initialization of the @loc@ field
    -- using the @loc@ parameter.
  , astLoc_maybePrependFieldInit ::
         String    -- Initialize with this expression.
      -> [String]  -- Existing list of initializations.
      -> [String]

    -- | A simple copy-assignment of the @loc@ field or 'empty'.
  , astLoc_maybeFieldAsg :: Doc

    -- | The C++ (un-cv-qualified, unreferenced) type of the @location@ field.
    -- Throws if we do not store locations.
  , astLoc_storageClass  :: String

    -- | Declaration of a @loc@ field storing the location of a node.
    -- 'empty' if not needed.
  , astLoc_fieldDecl     :: Doc

    -- | Checks if a user-defined token tracks its position.
  , astLoc_isPositionalToken ::
         String  -- Token name.
      -> Bool

    -- | Namespace functions.
  , ast_nsutils :: NamespaceUtils
  }

-- | Initializes AST location utils suitably.
newAstUtils ::
     Options.SharedOptions  -- ^ BNFC invokation options.
  -> (String -> Bool)  -- ^ Checks if a token is defined as positional.
  -> AstUtils
newAstUtils opts isPositionalToken = case locKind of
  CppLocationsNone -> initial
    { astLoc_maybePrependParam     = const id
    , astLoc_maybeParam            = const ""
    , astLoc_maybePrependFieldInit = const id
    , astLoc_maybeFieldAsg         = empty
    , astLoc_storageClass          = error "Storage of locations undefined"
    , astLoc_fieldDecl             = empty
    , ast_nsutils                  = nsutils
    }
  CppLocationsStart -> finalize "position" initial
  CppLocationsRange -> finalize "location" initial
  where
    locKind = getLocationKind opts
    nsutils = newNamespaceUtilsFromOptions opts
    initial = AstUtils
      { astLoc_locKind               = locKind
      , astLoc_isPositionalToken     = isPositionalToken
      , astLoc_maybePrependParam     = undefined
      , astLoc_maybeParam            = undefined
      , astLoc_maybePrependFieldInit = undefined
      , astLoc_maybeFieldAsg         = undefined
      , astLoc_storageClass          = undefined
      , astLoc_fieldDecl             = undefined
      , ast_nsutils                  = nsutils
      }
    finalize storage utils = utils
      { astLoc_maybePrependParam     = maybePrependParam
      , astLoc_maybeParam            =
        intercalate ", " . flip maybePrependParam [""]
      , astLoc_maybePrependFieldInit = \ s -> (("loc(" ++ s ++ ")") : )
      , astLoc_maybeFieldAsg         = text "loc = other.loc;"
      , astLoc_storageClass          = storage
      , astLoc_fieldDecl             = text $ storage ++ " loc;"
      }
      where
        maybePrependParam name = (("const " ++ storage ++ "&" ++
            (if null name
              then ""
              else " ")
            ++ name
            ) : )

------------------------------------------------------------------------
-- * Handling type completeness (using reordering and pointers).
------------------------------------------------------------------------

{- EXPLANATION
The abstract syntax file includes:
  * forward-declarations, like @class A;@;
  * full declarations, like @class A { public: int field; void method(); };@.

This backend generates 3 kinds of classes:
  * variants (from categories), as
      class Cat : public std::variant<L1, L2...> {};
  * normal classes (from labels, except "_", "(:)", "(:[])", "[]", and
    lower camel-case labels), as
      class Label {
      public:
        void SomeMethodsAndConstructors();
        std::unique_ptr<AnotherCategory> AnotherCategory_;
        ListCategory ListCategory_;
        // ^ note lack of std::unique_ptr on the ListCategory
      };
  * lists (from BNFC lists), as
      class ListCat : public std::deque<Cat> {};
      OR
      class ListCat : public std::deque<std::unique_ptr<Cat>> {};

In the end, we need all classes fully declared. But to fully declare a...
  * ...variant, we need __full declarations__ of all its /normal classes/;
  * ...normal class, we need __forward-declarations__ of all its member
    /variants/ and __full declarations__ of all its member /lists/.
  * ...list, we need a forward-declaration (if unique_ptr or a good STL
    implementation) OR a full declaration (if storing by value and a bad STL
    implementation) of its element class.

(There exist token structs, but we assume they are fully declared at the
start of the file and do not cause problems.)

However, we are always free to forward-declare a type.

Here is a diagram of dependencies if lists need...

  full declarations:       forward-declarations:

  VARIANT <---- LIST        VARIANT <.... LIST
   ^ |           ^           ^ |           ^      ----> needs a full decl.
   : |           |           : |           |
   : V           |           : V           |      ....> needs a partial decl.
  NORMAL --------+          NORMAL --------+

As we can see, a cycle is possible in the left case. Still, if there is no
cycle, we use a depth-first search to implement topological sorting of full
declarations.

Our strategy:
  * If --store-list-items-by=value:
      Assume at first that a full declaration is needed for list items
        (for maximum compatibility).
      If that fails, assume that only a forward-declaration is needed and
        still produce lists storing items by value. This will work with
        GNU std::deque which only needs complete types when we call the methods
        of std::deque.
  * If --store-list-items-by=pointer:
      Straightforward: list items only need forward-declarations.
  * If --store-list-items-by=prefer-value:
      Assume at first that a full declaration is needed for list items.
      If this succeeds, store items by value.
      If this fails, store items by pointer (with forward-declarations).
-}

-- | Representation of a to-be-generated class declaration.
-- Used in a list to specify the order of declarations.
data ClassDeclaration
  = ListClassDeclaration    !CF.Cat
    -- ^ A list class (BNFC list category).
    -- The stored t'BNFC.CF.Cat' is the list element,
    -- not the list category itself.
  | NormalClassDeclaration  !CF.Rule  -- ^ A normal class (BNFC label).
  | VariantClassDeclaration !String ![String]
    -- ^ A @std::variant@ type synonym (BNFC category).
  | ForwardDeclaration String         -- ^ Literally @class <name>;@.

data FullClassDeclaration
  = ListFullDeclaration    !CF.Cat
    -- ^ A list class (BNFC list category).
    -- The stored t'BNFC.CF.Cat' is the list element,
    -- not the list category itself.
  | NormalFullDeclaration  !CF.Rule  -- ^ A normal class (BNFC label).
  | VariantFullDeclaration !String ![String]
    -- ^ A @std::variant@ type synonym (BNFC category).

-- | Generalize a t'FullClassDeclaration' without changing.
full2justClassDecl :: FullClassDeclaration -> ClassDeclaration
full2justClassDecl = \case
  ListFullDeclaration    cat       -> ListClassDeclaration cat
  NormalFullDeclaration  rule      -> NormalClassDeclaration rule
  VariantFullDeclaration name vars -> VariantClassDeclaration name vars

-- | The main function. Produces a well-ordered list of declarations and decides
-- between v'StoreByValue' and v'StoreByPointer' if the user allows.
decideClassDeclarations ::
     MergedGroupedRules           -- ^ Grammar description.
  -> Options.ListItemStorageType  -- ^ User directive on how to store types.
  -> ([ClassDeclaration], ListItemStorage)
decideClassDeclarations grammar = \case
  Options.ItemsStoredAlwaysByValue    -> tryAndFallback StoreByValue
  Options.ItemsStoredByValueIfNoLoops -> tryAndFallback StoreByPointer
  Options.ItemsStoredAlwaysByPointer  ->
    case topsortClassDeclarations False topsortData of
      Nothing    -> error'
      Just order -> (order, StoreByPointer)
  where
    topsortData = prepareTopsortData $ getUnorderedClassDeclarations grammar
    tryAndFallback fallbackProducesStorageType =
      case topsortClassDeclarations True topsortData of
        Just order -> (order, StoreByValue)
        Nothing    -> case topsortClassDeclarations False topsortData of
          Nothing    -> error'
          Just order -> (order, fallbackProducesStorageType)
    error' = error $ "Cannot reorder class declarations in " ++ absynHppFilename

-- | Returns the appropriate class name for a declaration.
nameOfFullDecl :: FullClassDeclaration -> String
nameOfFullDecl = \case
  ListFullDeclaration elemCat   -> "List" ++ catNameWithCoerc elemCat
  NormalFullDeclaration rule    -> CF.funName rule
  VariantFullDeclaration name _ -> name

-- | Extracts a set of declarations to make.
getUnorderedClassDeclarations ::
     MergedGroupedRules  -- ^ Grammar description.
  -> [FullClassDeclaration]
getUnorderedClassDeclarations (MergedGroupedRules rulemap) =
  flip concatMap (Map.toList rulemap) $ \case
    (NontokenClass_ListCat elemCat, _) -> [ListFullDeclaration elemCat]
    (NontokenClass_Cat catname, rules) -> let
        namesAndRules =
          [ (name, rule)
          | rule <- rules, let name = CF.funName rule, isClassLabel name]
      in
        VariantFullDeclaration catname (map fst namesAndRules)
        : map (NormalFullDeclaration . snd) namesAndRules

-- The following data types and functions have "topsort" in their names. They
-- are all internal to the ordering algorithm.

-- | Has a class been fully declared?
data TopsortFullDeclarationState
  = Undeclared             -- ^ No.
  | ResolvingDependencies  -- ^ Currently recursing to define dependencies.
  | FullyDeclared          -- ^ Yes.

-- | Map: class index -> Has it been (at least) forward-declared?
type TopsortForwardDeclarationStates = IntMap Bool

-- | Map: class index -> Has it been completely declared?
type TopsortFullDeclarationStates = IntMap TopsortFullDeclarationState

-- | The DFS-based topological sorting algorithm changes the following:
data TopsortState = TopsortState
  { topsortState_fwd   :: TopsortForwardDeclarationStates
    -- ^ What types have been forward-declared above.
  , topsortState_fulld :: TopsortFullDeclarationStates
    -- ^ What types have been fully declared above, and which ones are currently
    -- being recursively visited.
  , topsortState_decls :: [ClassDeclaration]
    -- ^ The declarations above.
  }

-- | Local variables needed by the algorithm that do not change.
data TopsortPreparedData = TopsortPreparedData
  { topsortPreparedData_nDecls     :: !Int  -- ^ The number of classes.
  , topsortPreparedData_origArray  :: !(Array Int FullClassDeclaration)
    -- ^ The (unordered) classes array to reorder.
  , topsortPreparedData_declNames  :: !(Array Int String)
    -- ^ The class names. Used to resolve dependencies, therefore are unique.
  , topsortPreparedData_name2index :: !(Map String Int)
    -- ^ Maps class names to indices in the original array.
  }

-- | Precompute t'TopsortPreparedData'. We invoke the algorithm twice (unless
-- --store-list-items-by=pointer is specified); this saves computations.
prepareTopsortData :: [FullClassDeclaration] -> TopsortPreparedData
prepareTopsortData orig = TopsortPreparedData
  { topsortPreparedData_nDecls     = nDecls
  , topsortPreparedData_origArray  = origArray
  , topsortPreparedData_declNames  = declNames
  , topsortPreparedData_name2index = name2index
  }
  where
    nDecls    = length orig
    origArray = Array.listArray (0, nDecls - 1) orig
    declNames = Array.listArray (0, nDecls - 1) $ map nameOfFullDecl orig
    -- | We resolve classes by name, so we check that class names are unique.
    name2index :: Map String Int
    name2index
      | classNamesDuplicated = error
        $ "Duplicate class names in " ++ absynHppFilename
      | otherwise            = Map.fromDistinctAscList name2indexList
      where
        name2indexList = sort $ zip (Foldable.toList declNames) [0..]
        classNamesDuplicated = foldr ((||) . uncurry (==)) False
          $ namePairs $ map fst name2indexList
          where
            namePairs :: [String] -> [(String, String)]
            namePairs = \case
              []           -> []
              first : tail -> helper first tail
              where
                helper :: String -> [String] -> [(String, String)]
                helper cur = \case
                  [] -> []
                  nx : tail -> (cur, nx) : helper nx tail

-- | Reorder class declarations, possibly inserting forward-declarations, so
-- that the C++ compiler does not complain. Or report failure.
topsortClassDeclarations ::
     Bool
    -- ^ Does a list declaration need a __FULL__ element class declaration?
  -> TopsortPreparedData
    -- ^ Some precomputed variables.
  -> Maybe [ClassDeclaration]
topsortClassDeclarations listNeedsCompleteItems (TopsortPreparedData
  { topsortPreparedData_nDecls     = nDecls
  , topsortPreparedData_origArray  = origArray
  , topsortPreparedData_declNames  = declNames
  , topsortPreparedData_name2index = name2index
  }) = let
    finalState = foldr (\ classIndex -> \case
        Nothing    -> Nothing
        Just state -> dfsEnsureDeclared classIndex state)
      (Just TopsortState
        { topsortState_fwd   =
            IntMap.fromDistinctAscList $ zip [0..nDecls - 1] $ repeat False
        , topsortState_fulld =
            IntMap.fromDistinctAscList $ zip [0..nDecls - 1] $ repeat Undeclared
        , topsortState_decls = []
        }
        -- Indices reversed because @foldr@ is
        -- computed from right to left in this case.
      ) [nDecls - 1, nDecls - 2 .. 0]
  in case finalState of
    Nothing            -> Nothing
    Just TopsortState {topsortState_decls = decls} -> Just $ reverse decls
  where
    -- | The dependency graph (adjacency list): ([full deps], [fwd-deps])
    deps :: Array Int ([Int], [Int])
    deps = fmap getDeps origArray
      where
        makeListDependency :: Int -> ([Int], [Int])
        makeListDependency i
          | listNeedsCompleteItems = ([i], [])
          | otherwise              = ([], [i])
        getDeps = \case
          ListFullDeclaration elemCat -> case elemCat of
            CF.TokenCat _ -> ([], [])
            _             ->
              makeListDependency $ lookupName2index $ catNameNoCoerc elemCat
          NormalFullDeclaration rule ->
            foldr (\ cat res@(resFull, resFwd) -> case cat of
                CF.TokenCat _ -> res
                CF.ListCat _  ->
                  ( lookupName2index (catNameNoCoerc cat) : resFull
                  , resFwd
                  )
                _             ->
                  ( resFull
                  , lookupName2index (catNameNoCoerc cat) : resFwd
                  )
              ) ([], []) $ Either.lefts $ CF.rhsRule rule
          VariantFullDeclaration _ depNames ->
            (map lookupName2index depNames, [])

    dfsEnsureDeclared :: Int -> TopsortState -> Maybe TopsortState
    dfsEnsureDeclared classIndex state =
      case classIndex `doLookupIntMap` topsortState_fulld state of
        ResolvingDependencies -> Nothing  -- dependency loop
        FullyDeclared         -> Just state
        Undeclared            -> let
            lockedThis = state
              { topsortState_fulld =
                  IntMap.insert classIndex ResolvingDependencies
                  $ topsortState_fulld state
              }
            (myFullDeps, myFwdDeps) = deps ! classIndex
            stateWithFullDeps = foldr (\ depIndex -> \case
                Nothing     -> Nothing
                Just _state -> dfsEnsureDeclared depIndex _state
              ) (Just lockedThis) myFullDeps
            stateWithAllDeps =
              flip (foldr (\ depIndex -> ensureForwardDeclared depIndex))
                myFwdDeps <$> stateWithFullDeps
          in case stateWithAllDeps of
            Nothing -> Nothing
            Just (TopsortState
              { topsortState_fwd   = fwdState'
              , topsortState_fulld = fullDeclState'
              , topsortState_decls = decls'
              }) -> Just TopsortState
                { topsortState_fwd   =
                    IntMap.insert classIndex True fwdState'
                , topsortState_fulld =
                    IntMap.insert classIndex FullyDeclared fullDeclState'
                , topsortState_decls =
                    full2justClassDecl (origArray ! classIndex) : decls'
                }

    ensureForwardDeclared :: Int -> TopsortState -> TopsortState
    ensureForwardDeclared classIndex state@(TopsortState
        { topsortState_fwd   = fwdState
        , topsortState_fulld = fullDeclState
        , topsortState_decls = decls
        })
      | classIndex `doLookupIntMap` fwdState = state
      | otherwise                            = TopsortState
        { topsortState_fwd   = IntMap.insert classIndex True fwdState
        , topsortState_fulld = fullDeclState
        , topsortState_decls =
            ForwardDeclaration (declNames ! classIndex) : decls
        }

    lookupName2index name = case name `Map.lookup` name2index of
      Nothing -> error $ "name2index did not contain class name: " ++ name
      Just i  -> i
    doLookupIntMap :: Int -> IntMap a -> a
    doLookupIntMap i map = case IntMap.lookup i map of
      Nothing  -> error $ "Index " ++ show i ++ " not found in IntMap with "
        ++ show (IntMap.keys map)
      Just res -> res

------------------------------------------------------------------------
-- * Boilerplate.
------------------------------------------------------------------------

-- | @include@ directives at the top.
headerHead :: Doc
headerHead = unlinesToText [s|
#pragma once
#include <memory>
#include <string>
#include <deque>
#include <variant>
#include "Locations.hpp"
|]

-- | Declaration of templates in the @reflection@ namespace.
reflectionTemplates ::
     AstUtils
  -> Doc
reflectionTemplates AstUtils
  { astLoc_locKind = locKind
  } = unlinesToText [s|
template<class T> struct IsTokenStruct_t
{ static constexpr bool value = false; };
template<class T>
constexpr const bool IsTokenStruct = IsTokenStruct_t<T>::value;

template<class T> struct IsCategoryClass_t
{ static constexpr bool value = false; };
template<class T>
constexpr const bool IsCategoryClass = IsCategoryClass_t<T>::value;

template<class T> struct IsListClass_t
{ static constexpr bool value = false; };
template<class T>
constexpr const bool IsListClass = IsListClass_t<T>::value;

template<class T> struct IsLabelClass_t
{ static constexpr bool value = false; };
template<class T>
constexpr const bool IsLabelClass = IsLabelClass_t<T>::value;

template<class T> struct IsParserEntrypoint_t
{ static constexpr bool value = false; };
template<class T>
constexpr const bool IsParserEntrypoint = IsParserEntrypoint_t<T>::value;

template<class T> struct SupportsLocations_t
{ static constexpr bool value = false; };
template<class T>
constexpr const bool SupportsLocations = SupportsLocations_t<T>::value;

template<class T> struct CoercionLevel_t {};
template<class T> constexpr int CoercionLevel = CoercionLevel_t<T>::value;

template<class T> struct SyntaxNodeName_t {};
template<class T>
constexpr const char* SyntaxNodeName = SyntaxNodeName_t<T>::value;

#define REFL_NOCOERC(type, kind, loc) \
template<> struct Is##kind##_t<type> \
{ static constexpr bool value = true; }; \
template<> struct SyntaxNodeName_t<type> \
{ static constexpr const char* value = #type; }; \
template<> struct SupportsLocations_t<type> \
{ static constexpr bool value = loc; }

#define REFL(type, kind, loc, coerc) \
REFL_NOCOERC(type, kind, loc); \
template<> struct CoercionLevel_t<type> \
{ static constexpr int value = coerc; }
|]
  $++$ linesToText
    [ "#define REFL_VAR(type) REFL_NOCOERC(type, CategoryClass, " ++ haveLocations ++ ")"
    , "#define REFL_LABEL(type, coerc) REFL(type, LabelClass, " ++ haveLocations ++ ", coerc)"
    , "#define REFL_TOKEN(type, loc) REFL(type, TokenStruct, loc, 0)"
    , "#define REFL_LIST(type) REFL(type, ListClass, " ++ haveLocations ++ ", 0)"
    ]
  $++$ unlinesToText [s|
#define ENTRYPOINT(type) \
template<> struct IsParserEntrypoint_t<type> \
{ static constexpr bool value = true; }
|]
  where
    haveLocations = cppShow $ locKind /= CppLocationsNone

-- | Cleanup of reflection macros.
reflectionUndefs :: Doc
reflectionUndefs = unlinesToText [s|
#undef ENTRYPOINT
#undef REFL_LIST
#undef REFL_TOKEN
#undef REFL_LABEL
#undef REFL_VAR
#undef REFL
#undef REFL_NOCOERC
#undef REFL_KINDNAME
|]

-- | Static utility function: clones the contents of a @unique_ptr@,
-- putting the clone into another @unique_ptr@.
clonePtrImpl :: Doc
clonePtrImpl = linesToText
  [ "template <class T>"
  , "static std::unique_ptr<T> ClonePtr(const std::unique_ptr<T>& p) {"
  , "    if (!p) return {};"
  , "    return std::make_unique<T>(*p);"
  , "}"
  ]

-- | A template function that extracts the stored location from any supporting
-- AST node.
funcLocationOf :: Doc
funcLocationOf = unlinesToText [s|
// Requires location tracking. Returns an (optionally const) lvalue reference.
template<class T>
inline auto& LocationOf(T& node) {
    using PureT = std::decay_t<T>;
    static_assert(reflection::SupportsLocations<PureT>,
        "This class does not support location tracking");
    if constexpr (
            reflection::IsLabelClass<PureT>
            || reflection::IsTokenStruct<PureT>
            || reflection::IsListClass<PureT>)
        return node.loc;
    else  // CategoryClass
        return node.Location();
}
|]

------------------------------------------------------------------------
-- * Tokens.
------------------------------------------------------------------------

-- | Returns an appropriate name for a token field storing its value.
tokenStorageName ::
     String  -- ^ The name of the token class.
  -> String  -- ^ The name of the storage field.
tokenStorageName "Value" = "MyValue"
tokenStorageName _ = "Value"

-- | A pair of a @struct@ and its property definitions.
-- Used internally to return both from one function.
data StructWithReflection = StructWithReflection
  { structWithReflection_struct     :: !Doc  -- ^ The @struct@.
  , structWithReflection_reflection :: !Doc  -- ^ The property definitions.
  }

-- | Turns a list of t'StructWithReflection' into
-- a list of structs with a list of reflections.
unzipStructWithReflection ::
     [StructWithReflection]
  -> ([Doc], [Doc])
unzipStructWithReflection = foldr (\ StructWithReflection
    { structWithReflection_struct = struct
    , structWithReflection_reflection = refl
    } (ss, refls) -> (struct : ss, refl : refls)
  ) ([], [])

-- | Generates a declaration for a token structure with constructors and
-- assignment operators taking references to the storage type.
tokenStructWithRefConstructorsHeader ::
     AstUtils  -- ^ For location tracking.
  -> String    -- ^ The token name.
  -> String    -- ^ The data type (e.g. @std::string@ for a @String@ token).
  -> StructWithReflection
tokenStructWithRefConstructorsHeader locUtils name storageType
  = StructWithReflection
  { structWithReflection_struct = linesToText
    [ "struct " ++ name ++ " {"
    , "public:"
    ] $+$ nest 4 (locField' $+$ linesToText
      [ storageType ++ " " ++ tokenStorageName name ++ ";"
      , name ++ "() = default;"
      , name ++ "(const " ++ name ++ "&) = default;"
      , name ++ "(" ++ name ++ "&&) = default;"
      , name ++ "& operator=(const " ++ name ++ "&) = default;"
      , name ++ "& operator=(" ++ name ++ "&&) = default;"
      , name ++ "(" ++ maybeParam' ++ "const " ++ storageType ++ "&);"
      , name ++ "(" ++ maybeParam' ++ storageType ++ "&&);"
      , name ++ "& operator=(const " ++ storageType ++ "&);"
      , name ++ "& operator=(" ++ storageType ++ "&&);"
      ]) $+$ text "};"
  , structWithReflection_reflection =
    text $ "REFL_TOKEN(" ++ name ++ ", " ++ cppShow hasLocs ++ ");"
  }
  where
    AstUtils
      { astLoc_maybeParam        = maybeParam
      , astLoc_fieldDecl         = locField
      , astLoc_isPositionalToken = isPosToken
      } = locUtils
    hasLocs = isPosToken name
    maybeParam'
      | hasLocs   = maybeParam ""
      | otherwise = ""
    locField'
      | hasLocs   = locField
      | otherwise = empty

-- | Generates an implementation for a token structure with constructors and
-- assignment operators taking references to the storage type.
tokenStructWithRefConstructorsImpl ::
     AstUtils
  -> String  -- ^ The token name.
  -> String  -- ^ The data type (e.g. @std::string@ for a @String@ token).
  -> Doc
tokenStructWithRefConstructorsImpl locUtils name storageType =
  text ("// token: " ++ name)
  $++$ constructor ("const " ++ storageType ++ "& v") "(v)"
  $++$ constructor (storageType ++ "&& v") "(std::move(v))"
  $++$ linesToText
    [ name ++ "& " ++ name ++ "::operator=(const " ++ storageType ++ "& v) {"
    , "    " ++ tokenStorageName name ++ " = v;"
    , "    return *this;"
    , "}"
    , ""
    , name ++ "& " ++ name ++ "::operator=(" ++ storageType ++ "&& v) {"
    , "    " ++ tokenStorageName name ++ " = std::move(v);"
    , "    return *this;"
    , "}"
    ]
  where
    AstUtils
      { astLoc_maybeParam            = maybeParam
      , astLoc_maybePrependFieldInit = maybeLocInit
      , astLoc_isPositionalToken     = isPosToken
      } = locUtils
    hasLoc = isPosToken name
    maybeParam'
      | hasLoc    = maybeParam "loc"
      | otherwise = ""
    maybeLocInit'
      | hasLoc    = maybeLocInit "loc"
      | otherwise = id
    constructor valueparam valueinit =
      (text (name ++ "::" ++ name ++ "(" ++ maybeParam' ++ valueparam ++ ")")
      $+$ ctorInitializers (maybeLocInit' [tokenStorageName name ++ valueinit]))
        <> text " {}"

-- | Generates a declaration for a token structure with a by-value constructor
-- and assignment operator.
--
-- By-value tokens are currently never positional.
tokenStructHeader ::
     String  -- ^ The token name.
  -> String  -- ^ The data type (e.g. @std::string@ for a @String@ token).
  -> StructWithReflection
tokenStructHeader name storageType = StructWithReflection
  { structWithReflection_struct = linesToText
    [ "struct " ++ name ++ " {"
    , "public:"
    ] $+$ nest 4 (linesToText
      [ storageType ++ " " ++ tokenStorageName name ++ ";"
      , name ++ "() = default;"
      , name ++ "(const " ++ name ++ "&) = default;"
      , name ++ "& operator=(const " ++ name ++ "&) = default;"
      , name ++ "& operator=(" ++ name ++ "&&) = default;"
      , name ++ "(" ++ storageType ++ ");  /* implicit */"
      , name ++ "& operator=(" ++ storageType ++ ");"
      ]) $+$ text "};"
  , structWithReflection_reflection =
    text $ "REFL_TOKEN(" ++ name ++ ", false);"
  }

-- | Generates an implementation for a token structure with a by-value
-- constructor and assignment operator.
--
-- By-value tokens are currently never positional.
tokenStructImpl ::
     String  -- ^ The token name.
  -> String  -- ^ The data type (e.g. @std::string@ for a @String@ token).
  -> Doc
tokenStructImpl name storageType =
  linesToText
    [ "// token: " ++ name
    , ""
    , name ++ "::" ++ name ++ "(" ++ storageType ++ " v)"
    , "    : " ++ tokenStorageName name ++ "(v) {}"
    , ""
    , name ++ "& " ++ name ++ "::operator=(" ++ storageType ++ " v) {"
    , "    " ++ tokenStorageName name ++ " = v;"
    , "    return *this;"
    , "}"
    ]

-- | Generates declarations for all tokens.
headerTokens ::
     AstUtils
  -> [CF.Literal]  -- ^ The built-in tokens.
  -> [CF.Pragma]   -- ^ Contains user-defined tokens.
  -> StructWithReflection
headerTokens utils lits pragmas = StructWithReflection
  { structWithReflection_struct     = vcatSpaced structs
  , structWithReflection_reflection = foldr ($+$) empty reflections
  }
  where
    litTokens  = map makeLitToken lits
    makeHeader = tokenStructWithRefConstructorsHeader
    makeUserToken tkName =
      makeHeader utils tkName "std::string"
    userTokens =
      [ makeUserToken name
      | CF.TokenReg CF.WithPosition { wpThing = name} _ _ <- pragmas
      ]
    makeStringlikeToken s = makeHeader utils s "std::string"
    makeLitToken s
      | s == "Char"    = tokenStructHeader s "int32_t"
      | s == "String"  = makeStringlikeToken s
      | s == "Integer" = tokenStructHeader s "long"
      | s == "Double"  = tokenStructHeader s "double"
      | s == "Ident"   = makeStringlikeToken s
      | otherwise      = error $ "Unimplemented literal: " ++ s
    (structs, reflections) = unzipStructWithReflection (litTokens ++ userTokens)

-- | Generates implementations for all tokens.
implTokens ::
     AstUtils
  -> [CF.Literal]  -- ^ The built-in tokens.
  -> [CF.Pragma]   -- ^ Contains user-defined tokens.
  -> Doc
implTokens utils lits pragmas = vcatSpaced $ litTokens ++ userTokens
  where
    litTokens = map makeLitToken lits
    userTokens =
      [makeUserToken $ CF.wpThing name | CF.TokenReg name _ _ <- pragmas]
    makeStringlikeToken s =
      tokenStructWithRefConstructorsImpl utils s "std::string"
    makeLitToken s
      | s == "Char"    = tokenStructImpl s "int32_t"
      | s == "String"  = makeStringlikeToken s
      | s == "Integer" = tokenStructImpl s "long"
      | s == "Double"  = tokenStructImpl s "double"
      | s == "Ident"   = makeStringlikeToken s
      | otherwise      = error $ "Unimplemented literal: " ++ s
    makeUserToken s =
      tokenStructWithRefConstructorsImpl utils s "std::string"

------------------------------------------------------------------------
-- * Categories (nonterminals) and rules.
------------------------------------------------------------------------

-- | A record combining the declaration, property definitions, and
-- implementation of a rule label class.
data AbsynNodeCode = AbsynNodeCode
  { absynNodeCode_declaration    :: !Doc  -- ^ The class declaration.
  , absynNodeCode_reflection     :: !Doc  -- ^ The properties.
  , absynNodeCode_implementation :: !Doc  -- ^ The method implementations.
  }

-- | Generates declarations, reflection properties, and implementations
-- for all classes: categories, list categories, and rules.
defineAllClasses ::
     AstUtils
  -> ListItemStorage     -- ^ How to store list elements.
  -> [ClassDeclaration]  -- ^ Declarations to generate.
  -> AbsynNodeCode
defineAllClasses utils storeListItemsBy decls = foldr (\ decl code ->
    case decl of
      ListClassDeclaration elemCat ->
        vcatSpaced (listDef utils storeListItemsBy elemCat) code
      NormalClassDeclaration rule ->
        vcatSpaced (ruleDef utils rule) code
      VariantClassDeclaration name vars ->
        vcatSpaced (variantDef utils name vars) code
      ForwardDeclaration name -> code
        {absynNodeCode_declaration =
          text ("class " ++ name ++ ";") $+$ absynNodeCode_declaration code}
  ) AbsynNodeCode
  { absynNodeCode_declaration    = empty
  , absynNodeCode_reflection     = empty
  , absynNodeCode_implementation = empty
  } decls
  where
    vcatSpaced (AbsynNodeCode
      { absynNodeCode_declaration    = ldecl
      , absynNodeCode_reflection     = lrefl
      , absynNodeCode_implementation = limpl
      }) (AbsynNodeCode
      { absynNodeCode_declaration    = rdecl
      , absynNodeCode_reflection     = rrefl
      , absynNodeCode_implementation = rimpl
      }) = AbsynNodeCode
        { absynNodeCode_declaration    = ldecl $++$ rdecl
        , absynNodeCode_reflection     = lrefl $+$ rrefl
        , absynNodeCode_implementation = limpl $++$ rimpl
        }

-- | Generates code for a v'ListClassDeclaration'.
listDef ::
     AstUtils
  -> ListItemStorage  -- ^ How to store the elements.
  -> CF.Cat           -- ^ Type of elements.
  -> AbsynNodeCode
listDef AstUtils
  { astLoc_fieldDecl  = locFieldDecl
  , astLoc_maybeParam = locMaybeParam
  , astLoc_locKind    = locKind
  } storeBy elemCat =
  AbsynNodeCode
  { absynNodeCode_declaration = makeListDecl

  , absynNodeCode_reflection     =
    text $ "REFL_LIST(" ++ name ++ ");"

  , absynNodeCode_implementation = case storeBy of
    StoreByValue   -> empty
    StoreByPointer -> linesToText
      [ "// " ++ name
      , ""
      , name ++ "::" ++ name ++ "(const " ++ name ++ "& other) : deque() {"
      , "    for (auto& p : other) push_back(ClonePtr(p));"
      , "}"
      , ""
      , name ++ "& " ++ name ++ "::operator=(const " ++ name ++ "& other) {"
      , "    clear();"
      , "    for (auto& p : other) push_back(ClonePtr(p));"
      , "    return *this;"
      , "}"
      ]
  }
  where
    name     = "List" ++ catNameWithCoerc elemCat
    elemRawClass = catNameNoCoerc elemCat
    elemType = case storeBy of
      StoreByValue   -> elemRawClass
      StoreByPointer -> "std::unique_ptr<" ++ elemRawClass ++ ">"
    elemEmplaceWrap = case storeBy of
      StoreByValue   -> id
      StoreByPointer ->
        (("std::make_unique<" ++ elemRawClass ++ ">(") ++ ) . ( ++ ")")

    resLocAssignment = case locKind of
      CppLocationsNone -> empty
      _                -> text "res.loc = loc;"
    makeListDecl = linesToText
      [ concat
        ["class "
        , name
        , " : public std::deque<"
        , elemType
        , "> {"
        ]
      , "public:"
      , "    using deque::deque;"
      ] $+$ nest 4 (locFieldDecl $+$ case storeBy of
        StoreByValue   -> empty
        StoreByPointer -> linesToText
          [ concat
            [ name
            , "(const "
            , name
            , "& other);  /* clone */"
            ]
          , concat
            [ name
            , "("
            , name
            , "&& other) = default;"
            ]
          , concat
            [ name
            , "& operator=(const "
            , name
            , "& other);  /* discard & replace */"
            ]
          , concat
            [ name
            , "& operator=("
            , name
            , "&& other) = default;"
            ]
          ]
      $+$ linesToText
      [ "template <class... TItems>"
      , concat
        [ "static inline "
        , name
        , " Create("
        , locMaybeParam "loc"
        , "TItems&&... items) {"
        ]
      , "    " ++ name ++ " res;"
      ] $+$ resLocAssignment
      $+$ linesToText
      [ concat
        [ "    (res.emplace_back("
        , elemEmplaceWrap "std::forward<TItems>(items)"
        , "), ...);"
        ]
      , "    return res;"
      , "}"
      ] $+$ (case storeBy of
        StoreByValue   -> empty
        StoreByPointer -> linesToText
          [ "template <class... TItems>"
          , "static inline " ++ name
            ++ " CreateFromPointers(std::unique_ptr<TItems>&&... items) {"
          , "    " ++ name ++ " res;"
          , "    (res.emplace_back(std::move<std::unique_ptr<TItems>>"
            ++ "(items)), ...);"
          , "    return res;"
          , "}"
          ])
      ) $+$ text "};"

-- | Generates code for a v'VariantClassDeclaration'.
variantDef ::
     AstUtils
  -> String    -- ^ The name of the variant class.
  -> [String]  -- ^ The names of variants (labels).
  -> AbsynNodeCode
variantDef AstUtils
  { astLoc_locKind      = locKind
  , astLoc_storageClass = locStorageClass
  } name variants =
  AbsynNodeCode
  { absynNodeCode_declaration    = case variants of
    [singleVariant] -> classTop $+$ singleVariantBody singleVariant
    _               -> classTop
      $+$ (if hasLocation
        then nest 4 $ linesToText
          [ "const " ++ locStorageClass ++ "& Location() const;"
          , locStorageClass ++ "& Location();"
          ]
        else empty)
      $+$ text "};"
  , absynNodeCode_reflection     = text $ "REFL_VAR(" ++ name ++ ");"
  , absynNodeCode_implementation =
    if hasLocation && length variants /= 1
    then linesToText
      [ "// " ++ name
      , ""
      , "const " ++ locStorageClass ++ "& " ++ name ++ "::Location() const {"
      , "    return std::visit([](const auto& v) -> const "
        ++ locStorageClass ++ "& {"
      , "        return v.loc;"
      , "    }, *this);"
      , "}"
      , ""
      , "" ++ locStorageClass ++ "& " ++ name ++ "::Location() {"
      , "    return std::visit([](auto& v) -> " ++ locStorageClass ++ "& {"
      , "        return v.loc;"
      , "    }, *this);"
      , "}"
      ]
    else empty
  }
  where
    classTop = linesToText
      [ concat
        [ "class "
        , name
        , " : public std::variant<"
        , intercalate ", " variants
        , "> {"
        ]
      , "public:"
      , "    using variant::variant;"
      ]
    singleVariantBody singleVariant = nest 4 (linesToText
        [ "inline class " ++ singleVariant ++ "& " ++ singleVariant ++ "() {"
        , "    return std::get<class " ++ singleVariant ++ ">(*this);"
        , "}"
        , "inline const class " ++ singleVariant ++ "& "
          ++ singleVariant ++ "() const {"
        , "    return std::get<class " ++ singleVariant ++ ">(*this);"
        , "}"
        ]
        $+$ inlineLocGetters singleVariant)
      $+$ text "};"
    inlineLocGetters singleVariant
      | hasLocation = linesToText
        [ "inline const " ++ locStorageClass ++ "& Location() const {"
        , "    return std::get<class " ++ singleVariant ++ ">(*this).loc;"
        , "}"
        , "inline " ++ locStorageClass ++ "& Location() {"
        , "    return std::get<class " ++ singleVariant ++ ">(*this).loc;"
        , "}"
        ]
      | otherwise   = empty
    hasLocation = locKind /= CppLocationsNone

-- | Generates the declaration, properties, and implementation for a labeled
-- BNF rule.
ruleDef :: AstUtils -> CF.Rule -> AbsynNodeCode
ruleDef AstUtils
  { astLoc_maybePrependParam     = maybePrependLocParam
  , astLoc_maybePrependFieldInit = maybePrependLocInit
  , astLoc_maybeFieldAsg         = maybeLocFieldAsg
  , astLoc_fieldDecl             = locFieldDecl
  } r =
  let
    name = CF.funName r
    (indexedNames, members) = unzip $ fieldNames $ CF.rhsRule r
    storageTypes = map storageType' members
    normalTypes = map cat2typeName members
    constructorSignatureOrEmpty =
      let
        params = maybePrependLocParam "" (map ( ++ "&&") normalTypes)
      in
        if null params
        then empty
        else text $ name ++ "(" ++ intercalate ", " params ++ ");"
    headerClass = linesToText
      [ "class " ++ name ++ " {"
      , "public:"
      ] $+$ nest 4 (locFieldDecl $+$ linesToText
        [ name ++ "() = default;"
        , name ++ "(const " ++ name ++ "&);  /* clone */"
        , name ++ "(" ++ name ++ "&&) = default;"
        , name ++ "& operator=(const " ++ name ++ "&);  "
          ++ "/* discard & replace */"
        , name ++ "& operator=(" ++ name ++ "&&) = default;"
        ] $+$ constructorSignatureOrEmpty -- constructor
        -- Fields below
        $+$ foldr ($+$) empty
          (map (\ (typ, name) -> text (typ ++ " " ++ name ++ ";"))
          (zip storageTypes indexedNames)))
      $+$ text "};"

    headerRefls = text
      $ "REFL_LABEL(" ++ name ++ ", " ++ show (CF.precRule r) ++ ");"

    cloneValue storageCat value
      | isPointerType' storageCat =
        "ClonePtr<" ++ cat2typeName storageCat ++ ">(" ++ value ++ ")"
      | otherwise = value
    moveValue storageCat value
      | isPointerType' storageCat =
        "std::make_unique<" ++ cat2typeName storageCat
          ++ ">(std::move(" ++ value ++ "))"
      | otherwise = "std::move(" ++ value ++ ")"

    copyCtor =
      (text
      (name ++ "::" ++ name ++ "(const " ++ name ++ "& other [[maybe_unused]])")
      $+$ ctorInitializers (maybePrependLocInit "other.loc"
        [ name ++ "(" ++ cloneValue cat ("other." ++ name) ++ ")"
        | (name, cat) <- zip indexedNames members]))
      <> text " {}"
    copyAsg = text (name ++ "& " ++ name
             ++ "::operator=(const " ++ name ++ "& other [[maybe_unused]]) {")
      $+$ nest 4 (maybeLocFieldAsg $+$ linesToText
        [name ++ " = " ++ cloneValue cat ("other." ++ name) ++ ";"
        | (name, cat) <- zip indexedNames members]
        $+$ text "return *this;")
      $+$ text "}"
    ruleCtorOrEmpty =
      let
        params = maybePrependLocParam "loc"
          [ cat2typeName cat ++ "&& _" ++ show i
          | (cat, i :: Int) <- zip members [1..]]
        initializers = maybePrependLocInit "loc"
          [ name ++ "(" ++ moveValue cat ('_' : show i) ++ ")"
          | (name, cat, i :: Int) <- zip3 indexedNames members [1..]]
      in
        if null params
        then empty
        else
          (text (name ++ "::" ++ name ++ "(" ++ intercalate ", " params ++ ")")
          $+$ ctorInitializers initializers)
          <> text " {}"
    impl = vcatSpaced
      [ text $ "// " ++ name
      , copyCtor
      , copyAsg
      , ruleCtorOrEmpty
      ]

  in AbsynNodeCode
    { absynNodeCode_declaration    = headerClass
    , absynNodeCode_reflection     = headerRefls
    , absynNodeCode_implementation = impl
    }
  where
    cat2typeName :: CF.Cat -> String
    cat2typeName cat =
      maybeElaborateType (catNameNoCoerc cat) $ isClassType cat
    storageType' :: CF.Cat -> String
    storageType' cat
      | isPointerType' cat = "std::unique_ptr<" ++ typename ++ ">"
      | otherwise          = typename
      where
        typename = cat2typeName cat
    isPointerType' :: CF.Cat -> Bool
    isPointerType' = \case
      CF.CoercCat _ _ -> True
      CF.Cat      _   -> True
      _               -> False
    -- | If the type name contains an underscore, we want to be safe and specify
    -- that it is a class/struct and definitely not a different field.
    maybeElaborateType ::
         String  -- ^ Storage type name.
      -> Bool    -- ^ Is it a class? (As opposed to a struct).
      -> String  -- ^ The (possibly elaborated) type name.
    maybeElaborateType storageTypeName isClass
      | '_' `elem` storageTypeName =
        if isClass
        then "class " ++ storageTypeName
        else "struct " ++ storageTypeName
      | otherwise = storageTypeName
    isClassType :: CF.Cat -> Bool
    isClassType = \case
      CF.TokenCat _ -> False
      _             -> True

------------------------------------------------------------------------
-- * User-defined functions.
------------------------------------------------------------------------

-- | Generates user function headers.
declareFunctions ::
     AstUtils
  -> [CF.Pragma]  -- ^ Grammar pragmas (contain definitions).
  -> Doc
declareFunctions utils pragmas =
  entitleUserFunctions [declareFunction utils def | CF.FunDef def <- pragmas]

-- | Generates user function implementations.
translateFunctions ::
     AstUtils
  -> [CF.Pragma]  -- ^ Grammar pragmas (contain definitions).
  -> Doc
translateFunctions utils pragmas =
  entitleUserFunctions [translateFunction utils def | CF.FunDef def <- pragmas]

-- | Concatenates the function 'Doc's and prepends a header if the list is
-- not empty.
entitleUserFunctions ::
     [Doc]  -- ^ Function declarations or definitions.
  -> Doc    -- ^ Entitled file segment.
entitleUserFunctions = \case
    []       -> empty
    nonempty -> text "// User-defined functions" $++$ vcatSpaced nonempty

-- | Generates the header declaration for one user-defined function.
declareFunction ::
     AstUtils
  -> CF.Define  -- ^ The user-defined function.
  -> Doc
declareFunction utils = (<> text ";") . userFunctionSignature utils

-- | Generates the implementation for one user-defined function.
translateFunction ::
     AstUtils
  -> CF.Define -- ^ The user-defined function.
  -> Doc
translateFunction utils def =
  (userFunctionSignature utils def <> text " {") $+$ nest 4 body $+$ text "};"
  where
    hasLocation = astLoc_locKind utils /= CppLocationsNone
    isPosToken  = astLoc_isPositionalToken utils

    -- It is possible to define a function that uses one parameter several
    -- times:
    --   define dup arg1 = ExprPlus arg1 arg1;
    -- Since we use move semantics in every constructor (even in functions),
    -- we have to clone parameters manually.
    body = linesToText
      [ concat
        [ doLookupParamNameType origName
        , " "
        , cloneName
        , " = "
        , origName
        , ";"
        ]
      | (cloneName, origName) <- reverse reversedCloneDecls]
      $+$ (text "return " <> translateExpr (restoreLists dBody') <> text ";")

    CF.Define
      { defArgs = dParams
      , defBody = dBody
      } = def
    (_, _, dBody', reversedCloneDecls) =
      defineClones (getUsedFuncNamesInExpr dBody)
        (Set.fromList $ maybeAddLocParamName $ map fst dParams) dBody []
      where
        maybeAddLocParamName
          | hasLocation = (userFuncLocationParam : )
          | otherwise   = id

    mapParamNameType :: Map String String
    mapParamNameType = Map.fromList [(name, className t) | (name, t) <- dParams]
    doLookupParamNameType name = case name `Map.lookup` mapParamNameType of
      Nothing     -> error $ "Could not find parameter name " ++ name
        ++ " in " ++ show mapParamNameType
      Just clName -> clName

    -- | Requires an expr with restored lists (see restoreLists).
    translateExpr :: CF.Exp -> Doc
    translateExpr expr = case expr of
      CF.App funName (CF.FunT _ retType) args -> let
          cppArgs = map translateExpr args
        in callFunction retType funName cppArgs
        where
          callFunction ::
               CF.Base  -- ^ Expected return type.
            -> String   -- ^ Function/constructor name.
            -> [Doc]    -- ^ Arguments.
            -> Doc
          callFunction = \case
            CF.ListT _        -> callWrap hasLocation
            CF.BaseT typename -> \ fname ->
              if isClassLabel fname
              then
                if typename /= fname  -- equal for custom tokens
                then  -- variant(label(...))
                  callWrap False typename . (: []) . callWrap hasLocation fname
                else  -- customToken(...)
                  callWrap (hasLocation && isPosToken fname) fname
              else callWrap hasLocation ("make_" ++ fname)  -- function
      CF.Var      name -> callWrap False "std::move" [text   name]
      CF.LitInt    val -> callWrap False "Integer"   [text $ show   val]
      CF.LitDouble val -> callWrap False "Double"    [text $ show   val]
      CF.LitChar   val -> callWrap False "Char"      [text $ show $ ord val]
      CF.LitString val -> text $ cppShow val

    callWrap ::
         Bool    -- ^ Should we prepend a location argument?
      -> String  -- ^ Function name.
      -> [Doc]   -- ^ Arguments.
      -> Doc     -- ^ Function call.
    callWrap withLoc fname = (text (fname ++ "(") <> ) . ( <> text ")")
      . foldr ($+$) empty . punctuate comma
      . (if withLoc
        then (text userFuncLocationParam : )
        else id)

    restoreLists :: CF.Exp -> CF.Exp
    restoreLists = \case
      listExp@(CF.App _ (CF.FunT _ listType@(CF.ListT elemType)) _) ->
        let elems = map restoreLists $ restoreOneList listExp
        in
          CF.App
            (className listType ++ "::Create")
            (CF.FunT (map (const elemType) elems) listType)
            elems
      CF.App funName funType args -> CF.App funName funType
        $ map restoreLists args
      other                       -> other

    restoreOneList :: CF.Exp -> [CF.Exp]
    restoreOneList = \case
      CF.App "(:)" _ [argHead, argTail] -> argHead : restoreOneList argTail
      CF.App "[]"  _ _                  -> []
      _ -> error "Bad list representation in a user definition"

    getUsedFuncNamesInExpr :: CF.Exp -> Set String
    getUsedFuncNamesInExpr = \case
      CF.App name _ args ->
        Set.singleton name `Set.union`
          Set.unions (map getUsedFuncNamesInExpr args)
      _                  -> Set.empty
    defineClones ::
         Set String
      -> Set String
      -> CF.Exp
      -> [(String, String)]
      -> (Set String, Set String, CF.Exp, [(String, String)])
    defineClones usedNames unusedParams expr decls = case expr of
      CF.App funName funType args -> let
          (usedNames'', unusedParams'', decls'', args'') = foldr
            (\ arg (_usedNames, _unusedParams, _decls, res) -> let
                (usedNames', unusedParams', arg', decls') =
                  defineClones _usedNames _unusedParams arg _decls
              in
                (usedNames', unusedParams', decls', arg' : res)
            ) (usedNames, unusedParams, decls, []) args
        in (usedNames'', unusedParams'', CF.App funName funType args'', decls'')
      CF.Var name ->
        if name `Set.member` unusedParams
        then
          ( name `Set.insert` usedNames
          , name `Set.delete` unusedParams
          , expr
          , decls
          )
        else
          let (uniqName, newUsed) = getNewNameAndUpdateUsed name usedNames
          in (newUsed, unusedParams, CF.Var uniqName, (uniqName, name) : decls)
      _ -> (usedNames, unusedParams, expr, decls)
    getNewNameAndUpdateUsed :: String -> Set String -> (String, Set String)
    getNewNameAndUpdateUsed suggested used =
      let newname = getNewName suggested used
      in (newname, newname `Set.insert` used)
    getNewName :: String -> Set String -> String
    getNewName suggested used = helper 1
      where
        helper :: Int -> String
        helper i
          | name `Set.member` used = helper (i + 1)
          | otherwise              = name
          where
            name = suggested ++ "_" ++ show i

-- | The name given to the location parameter in user-defined functions.
userFuncLocationParam :: String
userFuncLocationParam = "__bnfc_loc__"

-- | Generates the signature (return type + name + parameters)
-- for one user-defined function.
userFunctionSignature ::
     AstUtils
  -> CF.Define  -- ^ The user-defined function.
  -> Doc
userFunctionSignature
  AstUtils
    { astLoc_maybePrependParam = maybePrependLocParam
    }
  (CF.Define
    { defName = name
    , defArgs = params
    , defType = retType
    }) = text $ concat
  [ className retType
  , " make_"
  , CF.wpThing name
  , "("
  , intercalate ", "
    $ maybePrependLocParam userFuncLocationParam
      [className t ++ "&& " ++ param | (param, t) <- params]
  , ")"
  ]

-- | Converts a BNFC type to its C++ class name
className ::
     CF.Base  -- ^ BNFC expression type.
  -> String
className = \case
  CF.BaseT s    -> s
  CF.ListT elem -> "List" ++ className elem

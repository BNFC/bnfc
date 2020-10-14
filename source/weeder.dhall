{ roots =
  [ "^Main.main$"
  -- Cabal-generated
  , "^Paths_.*"
  -- BNFC-generated
  , "^AbsBNF\\..*", "^LexBNF\\..*", "^ParBNF\\..*", "^SkelBNF\\..*", "^PrintBNF\\..*"
  -- CF (don't care)
  , "^Algebra\\..*", "^Data\\..*", "^Parsing\\..*"
  -- XML backend (don't care)
  , "Backend\\.XML\\..*"
  -- Utils (keep)
  , "^BNFC\\.Utils\\..*", "Backend\\.Haskell\\.Utils\\.tokenText"
  -- Used by doctest (keep)
  , "BNFC\\.Lexing\\.debugPrint", "BNFC\\.CF\\.np.*"
  ]
, type-class-roots = True
}

module Options where

data Target = TargetC | TargetCPP |TargetCPP_STL 
                | TargetCSharp | TargetFSharp |TargetHaskell |TargetHaskellGADT
                | TargetJava15 |TargetJava |TargetOCAML |TargetProfile
  deriving Eq

-- | Which version of Alex is targeted?
data AlexMode = Alex1 | Alex2 | Alex3 deriving Eq

data HappyMode = Standard | GLR deriving Eq

data SharedOptions = Options 
    { 
     targets :: [Target],
     make :: Bool,
     alexMode :: AlexMode,
     inDir :: Bool,
     shareStrings :: Bool,
     byteStrings :: Bool,
     glr :: HappyMode,
     xml :: Int,
     inPackage :: Maybe String, -- ^ The hierarchical package to put
	                        --   the modules in, or Nothing.
     lang :: String, -- ^ Prefix to use in module names
     multi :: Bool,
     cnf :: Bool -- ^ Generate CNF-like tables?
    }

anyTarget opts vs = any (isOpt opts) vs
  where isOpt     opts v  = elem v $ targets opts


    

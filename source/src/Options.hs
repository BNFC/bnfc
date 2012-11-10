module Options where

data AlexMode = Alex1 | Alex2 | Alex3 deriving Eq

data SharedOptions = Options 
    { 
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
    
    

data HappyMode = Standard | GLR deriving Eq

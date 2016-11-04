module BNFC.Backend.Haskell.CFtoHappySpec where

import Control.Monad (forM_)
import Data.String (IsString(..))

import Test.Hspec
import Text.PrettyPrint (render)

import BNFC.Backend.Haskell.CFtoHappy
import BNFC.Backend.Haskell.AbsHappy as Happy
import BNFC.Backend.Haskell.PrintHappy (printTree)
import BNFC.CF

rendersTo a b = render a `shouldBe` b
shouldGenerate a b = printTree a `shouldBe` b

spec = do

  describe "directives" $ do
    describe "mkParserName" $ do

      it "base case" $
        mkParserName False (Cat "A") `rendersTo` "%name pA A"
      it "list case" $
        mkParserName False (ListCat (Cat "A")) `rendersTo` "%name pListA ListA"
      it "functor case" $
        mkParserName True (Cat "A") `rendersTo` "%name pA_internal A"

  describe "convert" $ do
    it "quotes backslashes" $
      convert "\\" `rendersTo` "'\\\\'"

    it "quotes backslashes as part of a longer string" $
      convert "/\\" `rendersTo` "'/\\\\'"

  describe "getPosition" $ do

    it "returns 'Nothing' when the rule rhs is empty" $
      getPosition [] `shouldGenerate` "Nothing"

    it "uses `tokenLineCol` if the first item in rhs is a terminal" $
      getPosition [Right "bar"] `shouldGenerate` "Just (tokenLineCol $1)"

    it "uses `fst` if the item is a non-terminal" $
      getPosition [Left (Cat "Foo")] `shouldGenerate` "fst $1"

  describe "action" $ do
    let addRule = Rule "EPlus" (Cat "Exp") [Left (Cat "Exp"), Right "+", Left (Cat "Exp")]

    it "generates a simple action" $
      action "Abs" False [] addRule `shouldGenerate` "Abs.EPlus $1 $3"

    it "adds the position when using functons" $
      action "Abs" True []  addRule `shouldGenerate` "(fst $1, Abs.EPlus (fst $1)(snd $1)(snd $3))"

    let listA = ListCat (Cat "A")
        listB = ListCat (Cat "B")
        rule = Rule "Foo" (Cat "L") [Left listA, Right "+", Left listB]

    it "applies the reverse function for list that need reversing" $
      action "Abs" False [listB] rule `shouldGenerate` "Abs.Foo $1 (reverse $3)"

    it "applies snd and reverse in the right order" $
      action "Abs" True [listB] rule
      `shouldGenerate` "(fst $1, Abs.Foo (fst $1)(snd $1)(reverse (snd $3)))"

    it "doesn't prefix list constructors with the abstract module name" $
      action "Foo" False [] (Rule "(:)" (ListCat (Cat "A")) [Left (Cat "A"), Right",", Left (ListCat (Cat "A"))])
      `shouldGenerate` "(:) $1 $3"

    it "doesn't add position information to list constructors" $
      action "Foo" True [] (Rule "(:)" (ListCat (Cat "A")) [Left (Cat "A"), Right",", Left (ListCat (Cat "A"))])
      `shouldGenerate` "(fst $1, (:) (snd $1)(snd $3))"

    it "Generates action for coercions" $
      action "Foo" True [] (Rule "_" (Cat "Exp") [Right "(", Left (Cat "Exp"), Right ")"])
      `shouldGenerate` "(Just (tokenLineCol $1), snd $2)"

    it "flips the arguments for reversible lists" $
      let catA = Cat "A"
          listA = ListCat catA
          rule = Rule "(:)" listA [Left catA, Right",", Left listA]
      in action "Foo" False [listA] rule `shouldGenerate` "flip (:) $1 $2"

  describe "mkNonTerminal" $ do

    it "generates a non-terminal for a given category" $
      mkNonTerminal (Cat "Foo") `shouldBe` "Foo"

    it "prepends List to list categories" $
      mkNonTerminal (ListCat (Cat "Foo")) `shouldBe` "ListFoo"

    it "includes the precedence level" $
      mkNonTerminal (CoercCat "Foo" 3) `shouldBe` "Foo3"

  describe "mkTokenTerminal" $ do

    it "generates special legacy names for token categories" $
      sequence_
        [ mkTokenTerminal "Integer" `shouldBe` IdentTerm "L_integ"
        , mkTokenTerminal "Double"  `shouldBe` IdentTerm "L_doubl"
        , mkTokenTerminal "String"  `shouldBe` IdentTerm "L_quoted"
        , mkTokenTerminal "Char"    `shouldBe` IdentTerm "L_charac"
        , mkTokenTerminal "Ident"   `shouldBe` IdentTerm "L_ident"
        , mkTokenTerminal "MyTok"   `shouldBe` IdentTerm "L_MyTok"
        ]

  describe "mkTerminal" $ do

    it "escapes backslashes" $
      mkTerminal "\\" `shouldBe` "'\\\\'"

    it "escapes backslashes as part of a longer string" $
      mkTerminal "/\\" `shouldBe` "'/\\\\'"

    it "escapes single quotes" $
      mkTerminal "/\\" `shouldBe` "'/\\\\'"

  describe "mkPattern" $ do

    it "generates a simple pattern" $
      mkPattern [] (Rule "EPlus" (Cat "Exp") [Left (Cat "Exp"), Right "+", Left (Cat "Exp")])
      `shouldBe` [NonTerm "Exp", QuotedTerm "'+'", NonTerm "Exp"]

    it "reverses reversible lists" $
      let catA = Cat "A"
          listA = ListCat catA
          rule = Rule "(:)" listA [Left catA, Right ":", Left listA]
      in mkPattern [listA] rule
         `shouldBe` [NonTerm "ListA", NonTerm "A", QuotedTerm "':'"]

  describe "mkProductionType" $ do

    it "returns a single ident matching the name of the category if not using functor" $
      mkProductionType False (Cat "Exp") `shouldBe` TIdent "Exp"

    it "returns a list type for list categories" $
      mkProductionType False (ListCat (Cat "Exp")) `shouldBe` TList (TIdent "Exp")

    let typeMaybeIntInt = TApp (TIdent "Maybe") (TPair (TIdent "Int") (TIdent "Int"))

    it "returns a more complex type when using functor" $
      mkProductionType True (Cat "Exp")
        `shouldBe` TPair typeMaybeIntInt (TApp (TIdent "Exp") typeMaybeIntInt)

    it "returns a more complex type when using functor and a list cat" $
      mkProductionType True (ListCat (Cat "Exp"))
        `shouldBe` TPair typeMaybeIntInt (TList (TApp (TIdent "Exp") typeMaybeIntInt))

    it "when using a functor skips the type arguments for token categories" $
      mkProductionType True (TokenCat "Ident")
        `shouldBe` TPair typeMaybeIntInt (TIdent "Ident")

    it "testing the build-in tokens" $
      forM_ ["String", "Double", "Char", "Integer", "Ident"] $ \x ->
        mkProductionType False (TokenCat x) `shouldBe` TIdent (Ident x)

  describe "mkTokenAction" $ do

    context "base case" $ do
      it "String" $
        mkTokenAction False False False "String" `shouldBe` EIdent "$1"
      it "Integer" $
        mkTokenAction False False False "Integer" `shouldBe` EApp (EIdent "read") (EIdent "$1")
      it "Double" $
        mkTokenAction False False False "Double" `shouldBe` EApp (EIdent "read") (EIdent "$1")
      it "Char" $
        mkTokenAction False False False "Char" `shouldBe` EApp (EIdent "read") (EIdent "$1")
      it "Ident" $
        mkTokenAction False False False "Ident" `shouldBe` EApp (EIdent "Ident") (EIdent "$1")
      it "user-defined" $
        mkTokenAction False False False "MyTok" `shouldBe` EApp (EIdent "MyTok") (EIdent "$1")
      it "position token rule" $
        mkTokenAction True False False "MyTok" `shouldGenerate` "MyTok (mkPosToken $1)"

    context "using bytestrings" $ do
      it "String" $
        mkTokenAction False True False "String" `shouldGenerate` "BS.unpack $1"
      it "Integer" $
        mkTokenAction False True False "Integer" `shouldGenerate` "read (BS.unpack $1)"
      it "Double" $
        mkTokenAction False True False "Double" `shouldGenerate` "read (BS.unpack $1)"
      it "Char" $
        mkTokenAction False True False "Char" `shouldGenerate` "read (BS.unpack $1)"
      it "Ident" $
        mkTokenAction False True False "Ident" `shouldBe` EApp (EIdent "Ident") (EIdent "$1")
      it "user-defined" $
        mkTokenAction False True False "MyTok" `shouldBe` EApp (EIdent "MyTok") (EIdent "$1")
      it "position token rule" $
        mkTokenAction True True False "MyTok" `shouldGenerate` "MyTok (mkPosToken $1)"

    context "using functor" $ do
      it "String" $
        mkTokenAction False False True "String"
          `shouldGenerate` "(Just (tokenLineCol $1), prToken $1)"
      it "Integer" $
        mkTokenAction False False True "Integer"
          `shouldGenerate` "(Just (tokenLineCol $1), read (prToken $1))"
      it "user-defined" $
        mkTokenAction False False True "MyTok"
          `shouldGenerate` "(Just (tokenLineCol $1), MyTok (prToken $1))"

instance IsString Happy.Ident where
  fromString = Happy.Ident

instance IsString Happy.Terminal where
  fromString = Happy.Terminal

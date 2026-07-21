module BNFC.RegexMinusSpec where

import Test.Hspec
import BNFC.RegexMinus

-- | Lists matched prefix lengths in the STRICTLY ASCENDING order.
match :: SimpleRegex Char -> String -> [Int]
match = \case
  Term ch -> \case
    h : _  -> if h == ch then [1] else []
    _      -> []
  Lambda  -> const [0]
  Phi     -> const []
  Rep reg -> helper 0 []
    where
      helper dropped todo s =
        let
          matches = map (+dropped) $ matchNonempty reg s
          newtodo = merge todo matches
        in dropped : case newtodo of
          [] -> []
          nx : tail -> helper nx tail $ drop (nx - dropped) s
      matchNonempty r s = case match r s of
        0:tail -> tail
        other -> other
  Or  a b -> \ s -> match a s `merge` match b s
  Seq a b -> \ s -> let
      amatches = match a s
      amatchessuf = zip amatches $ sufs amatches s
    in mergeMany [map (+dropped) $ match b s | (dropped, s) <- amatchessuf]
  Sub _ _ -> error "No Sub promised, but one encountered"
  where
    merge :: [Int] -> [Int] -> [Int]
    merge = \case
      []          -> id
      as@(a : at) -> \case
        [] -> as
        bs@(b : bt) -> case a `compare` b of
          LT -> a : merge at bs
          GT -> b : merge as bt
          EQ -> a : merge at bt
    mergeMany :: [[Int]] -> [Int]
    mergeMany = foldr merge []
    sufs :: [Int] -> String -> [String]
    sufs = helper 0
      where
        helper prevlen lens prevsuf = case lens of
          [] -> []
          len : tail ->
            let newsuf = drop (len - prevlen) prevsuf
            in newsuf : helper len tail newsuf

-- | Checks if a string matches a regex.
matchFull :: SimpleRegex Char -> String -> Bool
matchFull reg s = length s `elem` match reg s

-- | Test cases.
spec :: Spec
spec = do
  let testcase function s expected =
        it ((if null s then "Empty" else s)
          ++ (if expected then " matches" else " does not match"))
        $ function s `shouldBe` expected

  -- Check that the 'matchFull' function works.
  describe "self-check" $ do
    let
      exp = Rep (Term 'a')
      test = testcase (matchFull exp)
    test "" True
    test "a" True
    test "aaA" False
    test "b" False
    test "aaaaaab" False
    test "aaaaaaa" True
    test "baaaaaa" False

  -- Example that requires loop detection. Taken From:
  -- http://home.chello.no/~mgrsby/sgmlintr/file0005.htm
  describe "(a+b)* - aa*" $ do
    let
      exp = removeMinuses $
        Sub (Rep (Or (Term 'a') (Term 'b')))
          (Seq (Term 'a') (Rep (Term 'a')))
      test = testcase (matchFull exp)
    test "" True
    test "a" False
    test "aaA" False
    test "b" True
    test "aaaaaab" True
    test "aaaaaaa" False
    test "baaaaaa" True

  describe "a-b" $ do
    let
      exp = removeMinuses $ Sub (Term 'a') (Term 'b')
      test = testcase (matchFull exp)
    test "a" True
    test "aa" False
    test "b" False
    test "" False
    test "c" False

  describe "(a+b) - b" $ do
    let
      exp = removeMinuses $ Sub (Or (Term 'a') (Term 'b')) (Term 'b')
      test = testcase (matchFull exp)
    test "" False
    test "a" True
    test "b" False
    test "ab" False
    test "aa" False
    test "bb" False
    test "ba" False

  describe "(a+b)c - b" $ do
    let
      exp = removeMinuses
        $ Sub (Seq (Or (Term 'a') (Term 'b')) (Term 'c'))
           (Term 'b')
      test = testcase (matchFull exp)
    test "" False
    test "ac" True
    test "bc" True
    test "a" False
    test "b" False
    test "c" False
    test "abc" False

  describe "(b*(a+b)c) - b" $ do
    let
      exp = removeMinuses
        $ Sub (Seq (Rep (Term 'b'))
              (Seq (Or (Term 'a') (Term 'b')) (Term 'c')))
           (Term 'b')
      test = testcase (matchFull exp)
    test "" False
    test "ac" True
    test "bc" True
    test "bac" True
    test "bbc" True
    test "bbac" True
    test "bbbc" True
    test "a" False
    test "b" False
    test "c" False
    test "acc" False
    test "bcc" False
    test "bacc" False
    test "bbcc" False
    test "bbacc" False
    test "bbbcc" False
    test "ab" False
    test "bb" False
    test "bab" False
    test "bbb" False
    test "bbab" False
    test "bbbb" False
    test "acb" False

  describe "[abcdefghijklmnopqrstuvwxyz]* - hello" $ do
    let
      exp = removeMinuses
        $ Sub (Rep (charset "abcdefghijklmnopqrstuvwxyz"))
           (string "hello")
      test = testcase (matchFull exp)
    test "hello" False
    test "" True
    test "a" True
    test "g" True
    test "sdcskjgnkjren" True
    test "helloworld" True
    test "ahello" True
    test "helloa" True
    test "_" False
    test "a b" False
    test "hell" True
    test "ello" True

  describe "[abcdef]* - fad" $ do
    let
      exp = removeMinuses $ Sub (Rep (charset "abcdef")) (string "fad")
      test = testcase (matchFull exp)
    test "fad" False
    test "" True
    test "a" True
    test "f" True
    test "deadbeef" True
    test "feeddad" True
    test "afad" True
    test "fada" True
    test "_" False
    test "a b" False
    test "fa" True
    test "ad" True

  describe "[abcdef]* - b" $ do
    let
      exp = removeMinuses $ Sub (Rep (charset "abcdef")) (string "b")
      test = testcase (matchFull exp)
    test "" True
    test "a" True
    test "b" False
    test "c" True
    test "d" True
    test "e" True
    test "f" True
    test "deadbeef" True
    test "feeddad" True
    test "ab" True
    test "ba" True
    test "_" False
    test "a d" False

  describe ("[a-z]* - [aeiouy]*") $ do
    let
      exp = removeMinuses
        $ Sub (Rep (charset "abcdefghijklmnopqrstuvwxyz"))
           (Rep (charset "aeiouy"))
      test = testcase (matchFull exp)
    test "" False
    test "abcd" True
    test "aaa" False
    test "bcd" True
    test "xfsckhzbtlpmgnrvqjwd" True
    test "yoaueaiueaieiau" False
    test "xfsckhzbtlpmagnrvqjwd" True
    test "yoaueaiueaiveiau" True

  describe "([a-z]* - [aeiouy]*) - bnfc" $ do
    let
      exp = removeMinuses
        $ Sub (Sub (Rep (charset "abcdefghijklmnopqrstuvwxyz"))
              (Rep (charset "aeiouy")))
           (string "bnfc")
      test = testcase (matchFull exp)
    test "" False
    test "abcd" True
    test "aaa" False
    test "bcd" True
    test "xfsckhzbtlpmgnrvqjwd" True
    test "yoaueaiueaieiau" False
    test "xfsckhzbtlpmagnrvqjwd" True
    test "yoaueaiueaiveiau" True
    test "bnfc" False
    test "nfc" True
    test "bnf" True

  describe "[a-z]* - ([bcdfghjklmnpqrstvwxz]* - bnfc)" $ do
    let
      exp = removeMinuses
        $ Sub (Rep (charset "abcdefghijklmnopqrstuvwxyz"))
              (Sub (Rep (charset "bcdfghjklmnpqrstvwxz"))
                   (string "bnfc"))
      test = testcase (matchFull exp)
    test "" False
    test "abcd" True
    test "bcd" False
    test "xfsckhzbtlpmgnrvqjwd" False
    test "bnfc" True
    test "nfc" False
    test "bnf" False

  describe "d(a*(bc)*) - d(ab(c)*+aa*+bc(a*))" $ do
    let
      exp = removeMinuses
        $ Sub (Seq (Term 'd')
                $ Rep (Term 'a') `Seq` Rep (Term 'b' `Seq` Term 'c'))
              (Seq (Term 'd')
                $ (Term 'a' `Seq` Term 'b' `Seq` Rep (Term 'c'))
                  `Or` (Term 'a' `Seq` Rep (Term 'a'))
                  `Or` (Term 'b' `Seq` Term 'c' `Seq` Rep (Term 'a')))
      test = testcase (matchFull exp)
    test "" False
    test "a" False
    test "d" True
    test "da" False
    test "daa" False
    test "dbc" False
    test "dbca" False
    test "dbcaa" False
    test "dab" False
    test "dabc" False
    test "dabcc" False
    test "dbcbc" True
    test "dabcbc" True
    test "daabc" True
    test "daabcbc" True
    test "dbca" False

  describe "a*(bc)* - aa*(bc*) = (bc)*" $ do
    let
      exp = removeMinuses
        ((Rep (Term 'a') `Seq` Rep (Term 'b' `Seq` Term 'c'))
         `Sub` (Term 'a' `Seq`
                  Rep (Term 'a') `Seq` Rep (Term 'b' `Seq` Term 'c')))
      test = testcase (matchFull exp)
    test "" True
    test "bc" True
    test "bcbc" True
    test "bcbcbc" True
    test "a" False
    test "abc" False
    test "abcbc" False
    test "aa" False
    test "aabc" False
    test "c" False
    test "b" False
    test "bcb" False
    test "bca" False
    test "bcabc" False

  describe "a*(bc)* - (a*|(bc)*) = aa*(bc)(bc)*" $ do
    let
      exp = removeMinuses
        ((Rep (Term 'a') `Seq` Rep (Term 'b' `Seq` Term 'c'))
         `Sub` (Rep (Term 'a') `Or` Rep (Term 'b' `Seq` Term 'c')))
      test = testcase (matchFull exp)
    test "" False
    test "a" False
    test "aa" False
    test "bc" False
    test "bcbc" False
    test "abc" True
    test "abcbc" True
    test "aabc" True
    test "cb" False
    test "acb" False
    test "ab" False

  describe "(a|b|c)* - (a|b|c)*ab(a|b|c)*" $ do
    let
      abcs = Rep $ charset "abc"
      exp = removeMinuses
        $ abcs `Sub` foldr Seq Lambda [abcs, string "ab", abcs]
      test = testcase (matchFull exp)
    test "" True
    test "abc" False
    test "a" True
    test "b" True
    test "c" True
    test "ab" False
    test "ac" True
    test "bc" True
    test "ba" True
    test "ca" True
    test "cb" True
    test "aa" True
    test "bb" True
    test "cc" True
    test "aaa" True
    test "bbb" True
    test "ccc" True
    test "aacaaa" True

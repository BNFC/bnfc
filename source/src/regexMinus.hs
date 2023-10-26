-- Written by David J. Sankel (camior@gmail.com)
--
-- License: Public Domain
--
-- Illustrates an algorithm to that converts a regular expression that uses
-- a difference operator (-) in to an equivalent one that does not.
--
-- Most ideas taken from http://home.chello.no/~mgrsby/sgmlintr/file0005.htm
--

import Data.List

data Regex a = Term a |
               Lambda | -- This is the 0-length string
               Phi |    -- Recognizes no strings ( 0 )
               Rep (Regex a) |  -- Kleen Star (*)
               Or (Regex a) (Regex a) | -- Or (+)
               Sub (Regex a) (Regex a) | -- Regex Subtraction (-)
               Seq (Regex a) (Regex a) -- Sequence (ab)
  deriving (Eq,Show)

-- Character set notation [asdf]
charset s = foldr Or Phi (map Term s)
-- Convenient seq notation "asdf"
string s = foldr Seq Lambda (map Term s)

-- Simple function used below
removeFirst p []  = []
removeFirst p (a:as) | p a = as
                     | otherwise = a:(removeFirst p as)

-- Takes a Regex and returns it without Subs.
removeMinuses (Term a) = Term a
removeMinuses Lambda = Lambda
removeMinuses Phi = Phi
removeMinuses (Rep a)= Rep a
removeMinuses (Or a b)= (Or a b)
removeMinuses (Seq a b)= (Seq a b)
removeMinuses e@(Sub a b) =
 let
   symbs = symbols e
   -- all (symbol,derivative) pairs
   derivatives = [(s,simplify $ derivative s e) | s <- symbs ]
   -- The delta function applied
   deltae = delta e
   -- Whether or not a (symbol,derivative)'s derivitive component is
   -- equal to our original formula
   loopCondition = (\(a,b) -> b == (simplify e))
   -- Maybe a (symbol,derivative) pair where the derivative is the same as our
   -- original expression
   loopedDerivative = find loopCondition derivatives
   -- all (symbol,derivative) pairs except for loopedDerivative
   derivativesMinusLooped = removeFirst loopCondition derivatives
   -- To understand the following: draw out a tree on a piece of paper.
   -- We are invoking the identity: R = (\Sigma_a aDaR) + \delta R.
   -- The loop condition recalls that if DaR = R then
   --   R = aDaR + stuff -> R = aR + stuff -> R = a*(stuff)
   result = case loopedDerivative of
     Nothing -> Or (foldr Or Phi [ (Seq (Term sym) (removeMinuses reg)) | (sym,reg) <- derivatives ]) deltae
     Just (sym,_) -> (Seq (Rep (Term sym))
                          (Or (foldr Or Phi [ (Seq (Term sym) (removeMinuses reg)) | (sym,reg) <- derivativesMinusLooped ]) deltae ) )
 in
   simplify result

-- Takes a Regex and returns an equivalent one somewhat simplified.
simplify (Term a) = (Term a)
simplify Lambda = Lambda
simplify Phi = Phi
simplify (Rep a) = let a_s = simplify a
                   in case a_s of
                        Lambda -> Lambda
                        Phi -> Lambda
                        _ -> (Rep a_s)
simplify (Or a b) = let a_s = simplify a
                        b_s = simplify b
                    in case (a_s,b_s) of
                         (_,Phi)-> a_s
                         (Phi,_) -> b_s
                         (Lambda,Lambda) -> Lambda
                         (_,Lambda) | delta a_s == Lambda -> a_s
                                    | otherwise -> (Or a_s b_s)
                         (Lambda,_) | delta b_s == Lambda -> b_s
                                    | otherwise -> (Or a_s b_s)
                         _ -> (Or a_s b_s)
simplify (Seq a b) = let a_s = simplify a
                         b_s = simplify b
                     in case (a_s,b_s) of
                          (_,Lambda)-> a_s
                          (Lambda,_) -> b_s
                          (_,Phi)-> Phi
                          (Phi,_) -> Phi
                          _ -> (Seq a_s b_s)
simplify (Sub a b) = let a_s = simplify a
                         b_s = simplify b
                     in case (a_s,b_s) of
                          (Lambda,Lambda) -> Phi
                          (_,Lambda) | delta a_s == Phi -> a_s
                                     | otherwise -> (Sub a_s b_s)
                          (Lambda,_) | delta b_s == Phi -> Lambda
                                     | otherwise -> Phi
                          (Phi,_) -> Phi
                          (_,Phi) -> a_s
                          _ -> (Sub a_s b_s)

-- Special Regex delta function.
-- See http://home.chello.no/~mgrsby/sgmlintr/file0004.htm
delta :: (Eq a) => (Regex a) -> (Regex a)
delta (Term a) = Phi
delta (Lambda) = Lambda
delta (Phi) = Phi
delta (Or a b) | ((delta a) == Phi) &&
                 ((delta b) == Phi) = Phi
               | otherwise = Lambda
delta (Sub a b) | ((delta a) == Lambda) &&
                  ((delta b) == Phi) = Lambda
                | otherwise = Phi
delta (Seq a b) | ((delta a) == Lambda) &&
                  ((delta b) == Lambda) = Lambda
                | otherwise = Phi
delta (Rep _) = Lambda

-- Special Regex derivative function.
-- See http://home.chello.no/~mgrsby/sgmlintr/file0004.htm
derivative :: (Eq a) => a -> Regex a -> Regex a
derivative a (Term b) | a == b = Lambda
                      | otherwise = Phi
derivative a Lambda = Phi
derivative a Phi = Phi
derivative a (Or b c) = Or (derivative a b) (derivative a c)
derivative a (Sub b c) = Sub (derivative a b) (derivative a c)
derivative a (Seq b c) = let dab = (derivative a b)
                             dac = (derivative a c)
                         in Or (Seq dab c)
                               (Seq (delta b) dac)
derivative a (Rep b) = Seq (derivative a b) (Rep b)

-- The symbols that a regex is over
symbols (Term a) = [a]
symbols (Rep a) = symbols a
symbols (Or a b) = nub $ symbols a ++ symbols b
symbols (Sub a b) = nub $ symbols a ++ symbols b
symbols (Seq a b) = nub $ symbols a ++ symbols b
symbols _ = []

-- Shows a Regex in compact form. For example: (a+b+c)d*
compactShow :: Regex Char -> String
compactShow (Term a) = [a]
compactShow Lambda = "\\"
compactShow Phi = "0"
compactShow (Rep (Term a)) = a:"*"
compactShow (Rep a) = "(" ++ (compactShow a) ++ ")*"
compactShow (Sub a b ) = (compactShow a) ++ "-" ++ (compactShow b)
compactShow (Or (Term a) (Term b) ) = [a] ++ "+" ++ [b]
compactShow (Or (Term a) b@(Or _ _) ) = [a] ++ "+" ++ (compactShow b)
compactShow (Or a@(Or _ _) (Term b) ) = (compactShow a) ++ "+" ++ [b]
compactShow (Or a@(Or _ _) b@(Or _ _) ) = (compactShow a) ++ "+" ++ (compactShow b)
compactShow (Or a b@(Or _ _) ) = "(" ++ (compactShow a) ++ ")+" ++ (compactShow b)
compactShow (Or a b ) = "(" ++ (compactShow a) ++ ")+(" ++ (compactShow b) ++ ")"
compactShow (Seq (Term a) (Term b) ) = [a] ++ [b]
compactShow (Seq (Term a) b@(Seq _ _) ) = [a] ++ (compactShow b)
compactShow (Seq (Term a) b@(Rep _) ) = [a] ++ (compactShow b)
compactShow (Seq a@(Seq _ _) b@(Seq _ _) ) = (compactShow a) ++ (compactShow b)
compactShow (Seq a b ) = "(" ++ (compactShow a) ++ ")" ++ "(" ++ (compactShow b) ++ ")"

-- Example that requires loop detection. Taken From:
--  http://home.chello.no/~mgrsby/sgmlintr/file0005.htm
-- (a+b)* - aa*
test = Sub (Rep (Or (Term 'a') (Term 'b') )) (Seq (Term 'a') (Rep (Term 'a')) )
-- a-b
test2 = Sub (Term 'a') (Term 'b')
-- (a+b) - b
test3 = Sub (Or (Term 'a') (Term 'b')) (Term 'b')
-- (a+b)c - b
test4 = Sub (Seq (Or (Term 'a') (Term 'b')) (Term 'c')) (Term 'b')
-- (b*(a+b)c) - b
test5 = Sub (Seq (Rep (Term 'b'))(Seq (Or (Term 'a') (Term 'b')) (Term 'c'))) (Term 'b')
-- Some normal looking string regexes.
test6 = Sub (Rep (charset "abcdefghijklmnopqrstuvwxyz")) (string "hello")
test7 = Sub (Rep (charset "abcdef")) (string "fad")
test8 = Sub (Rep (charset "abcdef")) (string "db")
-- Simplified Case of above
test9 = Sub (Rep (charset "abcdef")) (string "b")

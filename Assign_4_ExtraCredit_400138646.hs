-- Assignment 4 Extra Credit
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/11/07

module PolyDiff where

import Test.QuickCheck
import Text.Regex

data Poly a =
      X
    | Coef a
    | Sum (Poly a) (Poly a)
    | Prod (Poly a) (Poly a)
    deriving Show

instance Eq a => Eq (Poly a) where
    X            == X            = True
    (Coef c1)    == (Coef c2)    = c1 == c2
    (Sum p1 p2)  == (Sum p3 p4)  = p1 == p3 && p2 == p4 -- Constructors match -> call == recursively on values
    (Prod p1 p2) == (Prod p3 p4) = p1 == p3 && p2 == p4 -- Constructors match -> call == recursively on values
    _            == _            = False -- If it reaches this line, it has mismatched constructors


polyParse :: String -> [Integer]
polyParse str = let
    formatted = polyFormat str
    split = splitRegex (mkRegex "\\ *\\+\\ *") formatted
   in polyMergeTerms (map polyParseTerm split)

polyFormat :: String -> String
polyFormat str = subRegex (mkRegex "\\ *\\-\\ *") str " + -"

polyParseTerm :: String -> (Integer, Integer)
polyParseTerm "" = (0, 0)
polyParseTerm termStr = let
    split = splitRegex (mkRegex "x") termStr
    coef = read $ head split :: Integer
    pwr = if length split == 1 then 0 else
            if (length $ last split) == 0 then 1 else read $ tail $ last split
   in (pwr, coef)








{-
Takes a list of polynomial terms in tuple form (pwr, coef),
converts each term to list form, and then takes the vector
sum (with addZip) to produce a single polynomial in list form.
-}
polyMergeTerms :: Num a => [(Integer, a)] -> [a]
polyMergeTerms (t1:t2:ts) = let
    tl1 = polyTermToList t1
    tl2 = polyTermToList t2
   in addZip (addZip tl1 tl2) (polyMergeTerms ts)
polyMergeTerms [t1] = polyTermToList t1
polyMergeTerms []   = []

{-
Converts a 1-term polynomial into its list representation
The term must be represented as a tuple of term power and coefficient
-}
polyTermToList :: Num a => (Integer, a) -> [a]
polyTermToList (pwr, coef) = [if pwr == p then coef else 0 | p <- [0..pwr]]

{-
A variant of zipWith (+) [a] [a] that behaves as if the shorter lists
are padded by zeros
-}
addZip :: Num a => [a] -> [a] -> [a]
addZip (x:xs) (y:ys) = [x+y] ++ addZip xs ys
addZip (x:xs) []     = [x] ++ addZip (xs) []
addZip [] (y:ys)     = [y] ++ addZip [] (ys)
addZip [] []         = []

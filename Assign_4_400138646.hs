-- Assignment 4
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/11/06

module PolynomialList where

import Test.QuickCheck

data Poly =
      X
    | Coef Integer
    | Sum Poly Poly
    | Prod Poly Poly
    deriving Show

instance Eq Poly where
    X            == X            = True
    (Coef c1)    == (Coef c2)    = c1 == c2
    (Sum p1 p2)  == (Sum p3 p4)  = p1 == p3 && p2 == p4 -- Constructors match -> call == recursively on values
    (Prod p1 p2) == (Prod p3 p4) = p1 == p3 && p2 == p4 -- Constructors match -> call == recursively on values
    _            == _            = False -- If it reaches this line, it has mismatched constructors

getPolyList :: FilePath -> IO [Integer]
getPolyList path = do str <- readFile path
                      return (strToPolyList str)

strToPolyList :: String -> [Integer]
strToPolyList str = map read (lines str) :: [Integer]

polyListValue :: [Integer] -> Integer -> Integer
polyListValue (x:xs) n = x + n*(polyListValue xs n)
polyListValue [] n     = 0

polyListDegree :: [Integer] -> Integer
polyListDegree (x:xs) = toInteger $ length xs

polyListDeriv :: [Integer] -> [Integer]
polyListDeriv []   = []
polyListDeriv [x]  = []
polyListDeriv poly = let
    x = last poly
    xs = init poly
   in (polyListDeriv xs) ++ [x * (toInteger (length xs))]

polyListProd :: [Integer] -> [Integer] -> [Integer]
polyListProd p1 p2 = let
   polyTerms1 = polyListToTerms p1
   polyTerms2 = polyListToTerms p2
  in polyMergeTerms (polyTermsProd polyTerms1 polyTerms2)

polyListToTerms :: [Integer] -> [(Integer, Integer)]
polyListToTerms []   = []
polyListToTerms poly = let
   x = last poly
   xs = init poly
  in [(toInteger (length xs), x)] ++ (polyListToTerms xs)

polyTermsProd :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
polyTermsProd [] p = []
polyTermsProd p [] = []
polyTermsProd [(pwr, coef)] p = map (polyTermProd (pwr, coef)) p
polyTermsProd p [(pwr, coef)] = map (polyTermProd (pwr, coef)) p
polyTermsProd ((pwr, coef):xs) p2 = (polyTermsProd [(pwr, coef)] p2) ++ (polyTermsProd xs p2)

polyTermProd :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
polyTermProd (p1, c1) (p2, c2) = (p1 + p2, c1 * c2)

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

polyListSum :: [Integer] -> [Integer] -> [Integer]
polyListSum p1 p2 = addZip p1 p2

{-
This function takes a list of integers and converts it to the recursive Poly definition
E.g., [1,2,3] -> 1 + 2x + 3x^2
-}
polyListToPoly :: [Integer] -> Poly
polyListToPoly [] = Coef 0
polyListToPoly poly = let
    c = last poly
    cs = init poly
   in Sum (Prod (Coef c) (termPower (length cs))) (polyListToPoly cs)
      where termPower 0 = Coef 1
            termPower p = Prod X (termPower (p - 1))

polyToPolyList :: Poly -> [Integer]
polyToPolyList (Prod p1 p2) = polyListProd (polyToPolyList p1) (polyToPolyList p2)
polyToPolyList (Sum p1 p2)  = polyListSum (polyToPolyList p1) (polyToPolyList p2)
polyToPolyList (Coef c)     = [c]
polyToPolyList X            = [0, 1]

{-
A variant of zipWith (+) [a] [a] that behaves as if the shorter lists
are padded by zeros
-}
addZip :: Num a => [a] -> [a] -> [a]
addZip (x:xs) (y:ys) = [x+y] ++ addZip xs ys
addZip (x:xs) []     = [x] ++ addZip (xs) []
addZip [] (y:ys)     = [y] ++ addZip [] (ys)
addZip [] []         = []

containsFalse :: [Bool] -> Bool
containsFalse [] = False
containsFalse (x:xs) = ((not x) || (containsFalse xs))

{-
Trims a list by removing trailing 0 elements
Ex: [0,1,2,3,0,0] becomes [0,1,2,3]
-}
trimList :: (Eq a, Num a) => [a] -> [a]
trimList = reverse . trim . reverse
    where trim (x:xs) = if x /= 0 then x:xs else trim xs
          trim []     = []

polyListProdProp1 :: ([Integer], [Integer]) -> Bool
polyListProdProp1 (p1, p2) = let
    prod = polyListProd p1 p2
   in not (containsFalse [(polyListValue p1 n) * (polyListValue p2 n) == (polyListValue prod n) | n <- [-10..10]])

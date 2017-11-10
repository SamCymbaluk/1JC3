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

polyListSum :: [Integer] -> [Integer] -> [Integer]
polyListSum p1 p2 = addZip p1 p2

polyListProd :: [Integer] -> [Integer] -> [Integer]
polyListProd p1 p2 = let
   polyTerms1 = polyListToTerms p1
   polyTerms2 = polyListToTerms p2
  in polyMergeTerms (polyTermsProd polyTerms1 polyTerms2)

{-
This function takes a list of integers and converts it to the recursive Poly definition
E.g., [1,2,3] -> 1 + 2x + 3x^2
-}
polyListToPoly :: [Integer] -> Poly
polyListToPoly = listToPoly . reverse
    where listToPoly []     = Coef 0
          listToPoly (c:cs) = Sum (Prod (Coef c) (termPower (length cs))) (listToPoly cs)
            where termPower p = if p == 0 then Coef 1
                                else Prod X (termPower (p - 1))

{-
Determines the standard form of a given Poly of arbitrary structure.

The main process is to expand the polynomial fully, then collect like terms (terms with the same power).
-}
polyToPolyList :: Poly -> [Integer]
polyToPolyList = trimList . polyMergeTerms . polyCollectTerms . polyExpand

{-
Takes a polynomial of arbitrary structure and converts it to expanded form
Expanded form has the following property:
All Sums are above all Prods in the Poly tree; that is,
once you reach a Prod when traversing the tree downwards from the root,
you can be assured that there will be no more Sums.
-}
polyExpand :: Poly -> Poly
polyExpand p = let
    expand (Prod (Sum p1 p2) p) = Sum (Prod p p1) (Prod p p2) -- x(y + z) = xy + xz
    expand (Prod p (Sum p1 p2)) = Sum (Prod p p1) (Prod p p2)
    expand (Prod p1 p2) = Prod (polyExpand p1) (polyExpand p2) -- Prod without a sum directly below
    expand (Sum p1 p2) = Sum (polyExpand p1) (polyExpand p2)
    expand p = p
    p' = expand p
   in if p == p' then p' else polyExpand p' -- Continue preforming polyExpand until fully expanded (input == output)

{-
Takes an expanded polynomial and returns a list of term tuples
A term tuple is represents a polynomial term as follows:
(power, coefficient)
Ex: 5x^3 -> (3, 5)
-}
polyCollectTerms :: Poly -> [(Integer, Integer)]
polyCollectTerms (Sum p1 p2) = (polyCollectTerms p1) ++ (polyCollectTerms p2) -- Sum -> two terms to handle separately
polyCollectTerms (Prod p1 p2) = [polyCollectTermsAux (Prod p1 p2) (0, 1)] -- Prod -> a single term to convert to tuple form
polyCollectTerms (Coef a) = [(0, a)]
polyCollectTerms X = [(1, 1)]

{-
Takes a single term of an expanded polynomial and returns the tuple form.
Tuple form is represents a polynomial term as follows:
(power, coefficient)
Ex: 5x^3 -> (3, 5)
-}
polyCollectTermsAux :: Poly -> (Integer, Integer) -> (Integer, Integer)
polyCollectTermsAux (Prod p1 p2) (pwr, coef) = (pwr + p1Pwr + p2Pwr, coef * p1Coef * p2Coef) -- Add powers and multiply coefficient
    where (p1Pwr, p1Coef) = polyCollectTermsAux p1 (0, 1)
          (p2Pwr, p2Coef) = polyCollectTermsAux p2 (0, 1)
polyCollectTermsAux (Coef c) (pwr, coef) = (pwr, coef * c)
polyCollectTermsAux X (pwr, coef) = (pwr + 1, coef)

polyListToTerms :: [Integer] -> [(Integer, Integer)]
polyListToTerms [] = []
polyListToTerms poly = let
  x = last poly
  xs = init poly
 in [(toInteger (length xs), x)] ++ (polyListToTerms xs)

polyTermProd :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
polyTermProd (p1, c1) (p2, c2) = (p1 + p2, c1 * c2)

polyTermsProd :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
polyTermsProd [] p = []
polyTermsProd p [] = []
polyTermsProd [(pwr, coef)] p = map (polyTermProd (pwr, coef)) p
polyTermsProd p [(pwr, coef)] = map (polyTermProd (pwr, coef)) p
polyTermsProd ((pwr, coef):xs) p2 = (polyTermsProd [(pwr, coef)] p2) ++ (polyTermsProd xs p2)

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

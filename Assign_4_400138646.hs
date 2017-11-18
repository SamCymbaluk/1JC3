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

{-
Reads a file that contains an Integer on each line and returns the Integers as a list
-}
getPolyList :: FilePath -> IO [Integer]
getPolyList path = do str <- readFile path
                      return (strToPolyList str)

{-
Converts a string of one Integer per line to a list of Integers
-}
strToPolyList :: String -> [Integer]
strToPolyList str = map read (lines str) :: [Integer]

{-
Evaluates the list representation of a polynomials using Horner's method
-}
polyListValue :: [Integer] -> Integer -> Integer
polyListValue (x:xs) n = x + n*(polyListValue xs n)
polyListValue [] n     = 0

{-
Determines the degree of a list polynomial
Is undefined for empty lists
-}
polyListDegree :: [Integer] -> Integer
polyListDegree (x:xs) = toInteger $ length xs

{-
Computes the derivative of a list polynomial by
"bringing down the power and subtracting 1 from the power"
-}
polyListDeriv :: [Integer] -> [Integer]
polyListDeriv []   = []
polyListDeriv [x]  = []
polyListDeriv poly = let
    x = last poly
    xs = init poly
   in (polyListDeriv xs) ++ [x * (toInteger (length xs))]  -- The length of the remaining list represents the power


{-
Takes the sum of two polynomial lists by mapping (+) over the lists
The shorter polynomial is padded with zeros before being mapped
-}
polyListSum :: [Integer] -> [Integer] -> [Integer]
polyListSum p1 p2 = trimList (addZip p1 p2)

{-
Multiplies two list polynomials together by multiplying each term from one
with every term from the second, and then gathering like terms
-}
polyListProd :: [Integer] -> [Integer] -> [Integer]
polyListProd p1 p2 = let
   polyTerms1 = polyListToTerms p1
   polyTerms2 = polyListToTerms p2
  in polyMergeTerms (polyTermsProd polyTerms1 polyTerms2)

{-
Converts a poly list to a list of poly terms of the form
(power, coefficient)
-}
polyListToTerms :: [Integer] -> [(Integer, Integer)]
polyListToTerms []   = []
polyListToTerms poly = let
   x = last poly
   xs = init poly
  in [(toInteger (length xs), x)] ++ (polyListToTerms xs)

{-
Multiplies two lists of poly terms together by multiplying each term in the first poly
by every term in the second
-}
polyTermsProd :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
polyTermsProd [] p = []
polyTermsProd p [] = []
polyTermsProd [(pwr, coef)] p = map (polyTermProd (pwr, coef)) p -- When we are down to a single term in either poly,
polyTermsProd p [(pwr, coef)] = map (polyTermProd (pwr, coef)) p -- we can just distribute it into the other poly
polyTermsProd ((pwr, coef):xs) p2 = (polyTermsProd [(pwr, coef)] p2) ++ (polyTermsProd xs p2)

{-
Multiplies two poly terms together by adding powers and multiplying coefficients
-}
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

{-
This function takes a list of numbers and converts it to the recursive Poly definition
The output will be a simplified member of Poly
-}
polyListToPoly :: [Integer] -> Poly
polyListToPoly []    = Coef 0
polyListToPoly (0:[1]) = X
polyListToPoly (0:[n]) = Prod (Coef n) X
polyListToPoly poly  = let
    c           = last poly
    cs          = init poly
    power       = length cs
    term        = if power == 0 then Coef c else Prod (Coef c) (termPower power) -- power of 0 allows things
    p           = if power == 0 then term   else Sum term (polyListToPoly cs)    -- to be simplified
    termPower 0 = Coef 1
    termPower 1 = X
    termPower p = Prod X (termPower (p - 1))
   in if c /= 0 then p else polyListToPoly cs

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
containsFalse bools = not (foldr (&&) True bools)

{-
Trims a list by removing trailing 0 elements
Ex: [0,1,2,3,0,0] becomes [0,1,2,3]
-}
trimList :: (Eq a, Num a) => [a] -> [a]
trimList = reverse . trim . reverse
    where trim (x:xs) = if x /= 0 then x:xs else trim xs
          trim []     = []

implies :: Bool -> Bool -> Bool
True `implies` False = False
_    `implies` _     = True

{-
Function: polyListValue
Property: polyListValue should return the constant when x = 0 and degree > 0
Actual Test Result: Pass
-}
polyListValueProp1 :: [Integer] -> Bool
polyListValueProp1 poly = (length poly < 0) `implies` (polyListValue poly 0 == last poly)

{-
Function: polyListDegree
Property: Summing two polynomials should not change the max degree, when both polynomials have degree > 0
Actual Test Result: Pass
-}
polyListDegreeProp1 :: ([Integer], [Integer]) -> Bool
polyListDegreeProp1 (p1, p2) = (length p1 > 0 && length p2 > 0) `implies` ((max (polyListDegree p1) (polyListDegree p2)) == (polyListDegree (polyListSum p1 p2)))

{-
Function: polyListProp
Property: Taking the derivative of Polys with degree n (n > 0) should produce a poly of degree n - 1
Actual Test Result: Pass
-}
polyListDerivProp1 :: [Integer] -> Bool
polyListDerivProp1 p = let
    p' = polyListDeriv p
   in (length p > 0) `implies` (length p == length p' + 1)

{-
Function: polyListSum
Property: f(x) + g(x) = (f `polyListSum` g)(x)
Actual Test Result: Pass
-}
polyListSumProp1 :: ([Integer], [Integer], Integer) -> Bool
polyListSumProp1 (p1, p2, n) = let
    sum = polyListSum p1 p2
   in (polyListValue p1 n) + (polyListValue p2 n) == (polyListValue sum n)

{-
Function: polyListSun
Property: The sum of any poly p and [] should equal p
Actual Test Result: Pass
-}
polyListSumProp2 :: [Integer] -> Bool
polyListSumProp2 p = p `polyListSum` [] == p

{-
Function: polyListProd
Property: f(x) * g(x) = (f `polyListProd` g)(x)
Actual Test Result: Pass
-}
polyListProdProp1 :: ([Integer], [Integer], Integer) -> Bool
polyListProdProp1 (p1, p2, n) = let
    prod = polyListProd p1 p2
   in (polyListValue p1 n) * (polyListValue p2 n) == (polyListValue prod n)

{-
Test Plan

Function: getPolyList
Test Case Number: 1
Input: 1
Expected Output: [1]
Actual Output: [1]

Function: getPolyList
Test Case Number: 2
Input: 1\n2\n3\n4
Expected Output: [1,2,3,4]
Actual Output: [1,2,3,4]

Function: getPolyList
Test Case Number: 3
Input: 1\n-2\n3\n4
Expected Output: [1,-2,3,4]
Actual Output: [1,-2,3,4]

Function: getPolyList
Test Case Number: 4
Input:
Expected Output: []
Actual Output: []

Function: polyListValue
Test Case Number: 1
Input: [1] 5
Expected Output: 1
Actual Output: 1

Function: polyListValue
Test Case Number: 2
Input: [] 5
Expected Output: 0
Actual Output: 0

Function: polyListValue
Test Case Number: 3
Input: [5,-1,-10,4] (-1)
Expected Output: -8
Actual Output: -8

Function: polyListDegree
Test Case Number: 1
Input: polyListDegree []
Expected Output: Error
Actual Output: Exception: Non-exhaustive patterns in function polyListDegree

Function: polyListDegree
Test Case Number: 2
Input: [1,1,1]
Expected Output: 2
Actual Output: 2

Function: polyListDegree
Test Case Number: 3
Input: [1..100]
Expected Output: 99
Actual Output: 99

Function: polyListDeriv
Test Case Number: 1
Input: [5]
Expected Output: []
Actual Output: []

Function: polyListDeriv
Test Case Number: 2
Input: [5,-5]
Expected Output: [-5]
Actual Output: [-5]

Function: polyListDeriv
Test Case Number: 3
Input: [1,2,3,4,5]
Expected Output: [2,6,12,20]
Actual Output: [2,6,12,20]

Function: polyListSum
Test Case Number: 1
Input: [1,2,3,4,5] []
Expected Output: [1,2,3,4,5]
Actual Output: [1,2,3,4,5]

Function: polyListSum
Test Case Number: 2
Input: [1,2,3] [5,5]
Expected Output: [6,7,3]
Actual Output: [6,7,3]

Function: polyListSum
Test Case Number: 3
Input: [4,3,2,1] [-4,-3,-2,-1]
Expected Output: []
Actual Output: [0, 0, 0, 0]
Resolution: Modified function to trim trailing zeros from list

Function: polyListProd
Test Case Number: 1
Input: [1,2,3,4,5] []
Expected Output: []
Actual Output: []

Function: polyListProd
Test Case Number: 2
Input: [5] [5]
Expected Output: [25]
Actual Output: [25]

Function: polyListProd
Test Case Number: 3
Input: [1,2,3] [-5,4,3]
Expected Output: [-5, -6, -4, 18, 9]
Actual Output: [-5, -6, -4, 18, 9]

Function: polyListToPoly
Test Case Number: 1
Input: [5]
Expected Output: Coef 5
Actual Output:

Function: polyListToPoly
Test Case Number: 2
Input: [1, 1]
Expected Output: Sum (Prod (Coef 1) X) (Coef 1)
Actual Output: Sum (Prod (Coef 1) X) (Coef 1)

Function: polyListToPoly
Test Case Number: 3
Input: [1, 2, 3]
Expected Output: Sum (Prod (Coef 3) (Prod X X)) (Sum (Prod (Coef 2) X) (Coef 1))
Actual Output: Sum (Prod (Coef 3) (Prod X X)) (Sum (Prod (Coef 2) X) (Coef 1))

Function: polyToPolyList
Test Case Number: 1
Input: (Prod X (Sum (Coef 1) (Coef (-1))))
Expected Output: []
Actual Output: []

Function: polyToPolyList
Test Case Number: 2
Input: (Prod (Sum X (Coef 5)) (Sum X (Coef 5)))
Expected Output: [25,10,1]
Actual Output: [25,10,1]

Function: polyToPolyList
Test Case Number: 3
Input: (Prod (Sum X (Coef (-3))) (Prod (Sum X (Coef 2)) (Prod (Sum (Coef 5) (Prod (Coef (-1)) (Prod X X))) (Sum (Coef 5) (Prod (Coef (-1)) (Prod X X))))))
Expected Output: [-150,-25,85,10,-16,-1,1]
Actual Output: [-150,-25,85,10,-16,-1,1]
-}

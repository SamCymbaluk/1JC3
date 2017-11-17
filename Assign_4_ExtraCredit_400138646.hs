{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
-- Assignment 4 Extra Credit
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/11/07

module PolyDiff where

import Test.QuickCheck
import Text.Regex
import Generic.Random.Generic
import GHC.Generics

data Poly a =
      X
    | Coef a
    | Sum (Poly a) (Poly a)
    | Prod (Poly a) (Poly a)
    deriving (Show, Generic)

instance Eq a => Eq (Poly a) where
    X            == X            = True
    (Coef c1)    == (Coef c2)    = c1 == c2
    (Sum p1 p2)  == (Sum p3 p4)  = p1 == p3 && p2 == p4 -- Constructors match -> call == recursively on values
    (Prod p1 p2) == (Prod p3 p4) = p1 == p3 && p2 == p4 -- Constructors match -> call == recursively on values
    _            == _            = False -- If it reaches this line, it has mismatched constructors

instance Arbitrary (Poly Integer) where
  arbitrary = genericArbitraryRec uniform `withBaseCase` return X

polyParse :: String -> [Integer]
polyParse str = let
    formatted = polyFormat str
    split = splitRegex (mkRegex "\\ *\\+\\ *") formatted
   in polyMergeLike (map polyParseTerm split)

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

getPoly :: FilePath -> IO (Poly Integer)
getPoly path = do str <- readFile path
                  return $ (polyListToPoly . polyParse) str

polyPrettyPrint :: Poly Integer -> String
polyPrettyPrint = polyTrimPrint . polyListPrettyPrint .  polyAsList

{-
Trim leading " + " from pretty-printed poly
-}
polyTrimPrint :: String -> String
polyTrimPrint "" = ""
polyTrimPrint str = let
    regex = (mkRegex "^\\ *\\+\\ *")
   in last (splitRegex regex str)

polyListPrettyPrint :: [Integer] -> String
polyListPrettyPrint [] = ""
polyListPrettyPrint poly = let
    c = show $ last poly
    cs = init poly
    power = length cs
    term = if c == "0" then ""
           else if power /= 0 then " + " ++ c ++ "x^" ++ (show power) else c
   in polyListPrettyPrint cs ++ term



polySimp :: Poly Integer -> Poly Integer
polySimp = polyListToPoly . polyAsList

getPolyAndDiff :: FilePath -> IO String
getPolyAndDiff path = do poly <- getPoly path
                         return $ (polyPrettyPrint . polyDeriv) poly

{-
Evaluate a numeric polynomial at a given value by
performing the operations the constructors represent
after plugging in the given value for X
-}
polyValue :: Num a => Poly a -> a -> a
polyValue X n            = n
polyValue (Coef c) n     = c
polyValue (Sum p1 p2) n  = (polyValue p1 n) + (polyValue p2 n)
polyValue (Prod p1 p2) n = (polyValue p1 n) * (polyValue p2 n)

polyListValue :: Num a =>  [a] -> a -> a
polyListValue (x:xs) n = x + n*(polyListValue xs n)
polyListValue [] n     = 0

{-
Determines the degree of the given polynomial such that,
when the polynomial is represented as
p(x) = a0*x^0 + a1*x^1 ... am*x^m, am != 0,
degree = m
-}
polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree p = let
    degree = length (polyAsList p) - 1
   in if degree >= 0 then toInteger degree else 0

{-
Performs differentiation on a polynomial p to produce p'
The following derivative properties are used:
(f + g)' = f' + g'
Product rule: (fg)' = f'g + g'f
Chain rule: (f(g(x)))' = f'(g(x)) * g'(x)
(d/dx) x = 1
(d/dx) c = 0
-}
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Sum p1 p2)  = Sum p1' p2' -- Derivative of a sum is the sum of the derivatives
    where p1' = polyDeriv p1
          p2' = polyDeriv p2
polyDeriv (Prod p1 p2) = Sum (Prod p1' p2) (Prod p2' p1) -- Product rule
    where p1' = polyDeriv p1 -- Recursively call polyDeriv
          p2' = polyDeriv p2 -- in order to preform chain rule
polyDeriv X            = Coef 1
polyDeriv (Coef _)     = Coef 0

{-
Determines the standard form of a given Poly of arbitrary structure.

The main process is to expand the polynomial fully, then collect like terms (terms with the same power).
-}
polyAsList :: (Num a, Eq a) => Poly a -> [a]
polyAsList = trimList . polyMergeLike . polyCollectLike . polyExpand

{-
Takes a polynomial of arbitrary structure and converts it to expanded form
Expanded form has the following property:
All Sums are above all Prods in the Poly tree; that is,
once you reach a Prod when traversing the tree downwards from the root,
you can be assured that there will be no more Sums.
-}
polyExpand :: (Eq a, Num a) => Poly a -> Poly a
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
polyCollectLike :: Num a => Poly a -> [(Integer, a)]
polyCollectLike (Sum p1 p2) = (polyCollectLike p1) ++ (polyCollectLike p2) -- Sum -> two terms to handle separately
polyCollectLike (Prod p1 p2) = [polyCollectLikeAux (Prod p1 p2) (0, 1)] -- Prod -> a single term to convert to tuple form
polyCollectLike (Coef a) = [(0, a)]
polyCollectLike X = [(1, 1)]

{-
Takes a single term of an expanded polynomial and returns the tuple form.
Tuple form is represents a polynomial term as follows:
(power, coefficient)
Ex: 5x^3 -> (3, 5)
-}
polyCollectLikeAux :: Num a => Poly a -> (Integer, a) -> (Integer, a)
polyCollectLikeAux (Prod p1 p2) (pwr, coef) = (pwr + p1Pwr + p2Pwr, coef * p1Coef * p2Coef) -- Add powers and multiply coefficient
    where (p1Pwr, p1Coef) = polyCollectLikeAux p1 (0, 1)
          (p2Pwr, p2Coef) = polyCollectLikeAux p2 (0, 1)
polyCollectLikeAux (Coef c) (pwr, coef) = (pwr, coef * c)
polyCollectLikeAux X (pwr, coef) = (pwr + 1, coef)

{-
Takes a list of polynomial terms in tuple form (pwr, coef),
converts each term to list form, and then takes the vector
sum (with addZip) to produce a single polynomial in list form.
-}
polyMergeLike :: Num a => [(Integer, a)] -> [a]
polyMergeLike (t1:t2:ts) = let
    tl1 = polyTermToList t1
    tl2 = polyTermToList t2
   in addZip (addZip tl1 tl2) (polyMergeLike ts)
polyMergeLike [t1] = polyTermToList t1
polyMergeLike []   = []

{-
Converts a 1-term polynomial into its list representation
The term must be represented as a tuple of term power and coefficient
-}
polyTermToList :: Num a => (Integer, a) -> [a]
polyTermToList (pwr, coef) = [if pwr == p then coef else 0 | p <- [0..pwr]]

{-
This function takes a list of integers and converts it to the recursive Poly definition
The output will be a simplified member of Poly
-}
polyListToPoly :: (Num a, Eq a) => [a] -> Poly a
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

{-
A variant of zipWith (+) [a] [a] that behaves as if the shorter lists
are padded by zeros
-}
addZip :: Num a => [a] -> [a] -> [a]
addZip (x:xs) (y:ys) = [x+y] ++ addZip xs ys
addZip (x:xs) []     = [x] ++ addZip (xs) []
addZip [] (y:ys)     = [y] ++ addZip [] (ys)
addZip [] []         = []


countOccurrences :: Eq a => [a] -> a -> Int
countOccurrences xs x = length (filter (x==) xs)

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
polyParse is the inverse of polyListPrettyPrint
-}
polyParseProp1 :: Poly Integer -> Bool
polyParseProp1 poly = polyParse (polyPrettyPrint poly) == polyAsList poly

{-
Pretty printing a poly should not produce more terms that the degree of the poly
-}
polyPrettyPrintProp1 :: Poly Integer -> Bool
polyPrettyPrintProp1 poly = let
    deg = polyDegree poly
    printed = polyPrettyPrint poly
   in deg >= (toInteger (countOccurrences printed '+'))

{-
Taking the derivative of a Poly with degree > 0 should decrease the degree by 1
-}
polyDerivProp1 :: Poly Integer -> Bool
polyDerivProp1 poly = let
    p' = polyDeriv poly
   in (polyDegree poly > 0) `implies` (polyDegree poly - 1 == polyDegree p')

{-
The list representation of a poly should not change after being simplified
-}
polySimpProp1 :: Poly Integer -> Bool
polySimpProp1 poly = polyAsList poly >= polyAsList (polySimp poly)

{-
Elegant and highly efficient recursive solution to determine evenness of numbers
/r/programmerhumor :)
-}
isEven :: Integer -> Bool
isEven 0 = True
isEven 1 = False
isEven n = isEven (n - 2)



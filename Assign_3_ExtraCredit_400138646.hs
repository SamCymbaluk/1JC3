-- Assignment 3 Extra Credit
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/10/21

module Polynomial where

import Test.QuickCheck

data Poly a =
      X
    | Coef a
    | Sum (Poly a) (Poly a)
    | Prod (Poly a) (Poly a)
    deriving Show

instance Eq a => Eq (Poly a) where
    X            == X            = True
    (Coef c1)    == (Coef c2)    = c1 == c2
    (Sum p1 p2)  == (Sum p3 p4)  = p1 == p3 && p2 == p4
    (Prod p1 p2) == (Prod p3 p4) = p1 == p3 && p2 == p4
    _            == _            = False -- If it reaches this line, it has mismatched constructors

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
Newton's method is a way of approximating roots to a function by
repeatedly preforming operations given an initial seed, that may
or may not convert on the root of the function

x2 = x1 - f(x1)/f'(x1)

This implementation preforms Newton's method until successive x values
are within a delta of 1e-5
-}
polyNewton :: (Fractional a, Ord a) => Poly a -> a -> a
polyNewton p s = let s' = s - (polyValue p s)/(polyValue (polyDeriv p) s)
                     epsilon = 0.00001
                  in if abs (s' - s) <= epsilon then s' else polyNewton p s'

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
polyCollectLike (Prod p1 p2) = [polyCollectLikeAux (Prod p1 p2) (0, 1)] -- Prod -> a single term convert to tuple form

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
Trims a list by removing trailing 0 elements
Ex: [0,1,2,3,0,0] becomes [0,1,2,3]
-}
trimList :: (Eq a, Num a) => [a] -> [a]
trimList = reverse . trim . reverse
    where trim (x:xs) = if x /= 0 then x:xs else trim xs
          trim []     = []

{-
This function takes a list of integers and converts it to the recursive Poly definition
E.g., [1,2,3] -> 1 + 2x + 3x^2
-}
polyFromList :: Num a => [a] -> Poly a
polyFromList = listToPoly . reverse
    where listToPoly []     = Prod (Coef 0) X
          listToPoly (c:cs) = Sum (Prod (Coef c) (termPower (length cs))) (listToPoly cs)
            where termPower p = if p == 0 then Coef 1
                                else Prod X (termPower (p - 1))

{-
Converts a polynomial into a string that can be interpreted by most calculators
Great for checking your functions work correctly!
-}
polyToStr :: Show a => Poly a -> String
polyToStr X            = "x"
polyToStr (Coef a)     = show a
polyToStr (Prod p1 p2) = "("++(polyToStr p1)++")" ++ "*" ++ "("++(polyToStr p2)++")"
polyToStr (Sum p1 p2)  = "("++(polyToStr p1)++")" ++ "+" ++ "("++(polyToStr p2)++")"

{-
A variant of zipWith (+) [a] [a] that behaves as if the shorter lists
are padded by zeros
-}
addZip :: Num a => [a] -> [a] -> [a]
addZip (x:xs) (y:ys) = [x+y] ++ addZip xs ys
addZip (x:xs) []     = [x] ++ addZip (xs) []
addZip [] (y:ys)     = [y] ++ addZip [] (ys)
addZip [] []         = []
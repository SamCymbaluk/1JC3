-- Assignment 3
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date Started 2017/10/21
-- Date Complete 2017/

module Polynomial where

data Poly =
      X
    | Coef Integer
    | Sum Poly Poly
    | Prod Poly Poly
    deriving Show

polyValue :: Poly -> Integer -> Integer
polyValue X n            = n
polyValue (Coef c) n     = c
polyValue (Sum p1 p2) n  = (polyValue p1 n) + (polyValue p2 n)
polyValue (Prod p1 p2) n = (polyValue p1 n) * (polyValue p2 n)

polyDegree :: Poly -> Integer
polyDegree (Sum p _)  = polyDegree p
polyDegree (Prod _ p) = polyDegree p + 1
polyDegree (Coef _)   = 0

polyDeriv :: Poly -> Poly
polyDeriv (Sum p1 p2)       = Sum (polyDeriv p1) (polyDeriv p2)
polyDeriv (Prod (Coef c) p) = Prod (Coef c) (polyDeriv p)
polyDeriv (Prod X p)        = Prod (Coef (polyDegree p + 1)) p
polyDeriv X                 = Coef 0
polyDeriv (Coef _)          = Coef 0

{-
This function takes a list of integers and converts it to the recursive Poly definition
E.g., [1,2,3] -> 1*x^2 + 2*x + 3
-}
listToPoly :: [Integer] -> Poly
listToPoly []     = Prod (Coef 0) X
listToPoly (c:cs) = Sum (Prod (Coef c) (termPower (length cs))) (listToPoly cs)
    where termPower p = if p == 0 then Coef 1
                        else Prod X (termPower (p - 1))





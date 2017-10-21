-- Assignment 3 Extra Credit
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date Started 2017/10/21
-- Date Complete 2017/

module Polynomial where

data Poly a =
      X
    | Coef a
    | Sum (Poly a) (Poly a)
    | Prod (Poly a) (Poly a)
    deriving Show

polyValue :: Num a => Poly a -> a -> a
polyValue X n            = n
polyValue (Coef c) n     = c
polyValue (Sum p1 p2) n  = (polyValue p1 n) + (polyValue p2 n)
polyValue (Prod p1 p2) n = (polyValue p1 n) * (polyValue p2 n)

polyDegree :: Poly a -> Integer
polyDegree (Sum p _)  = polyDegree p
polyDegree (Prod _ p) = polyDegree p + 1
polyDegree (Coef _)   = 0

polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Sum p1 p2)       = Sum (polyDeriv p1) (polyDeriv p2)
polyDeriv (Prod (Coef c) p) = Prod (Coef c) (polyDeriv p)
polyDeriv (Prod X p)        = Prod (Coef (fromIntegral (polyDegree p + 1))) p
polyDeriv X                 = Coef 0
polyDeriv (Coef _)          = Coef 0

polyAsList :: Num a => Poly a -> [a]
polyAsList (Sum p1 p2) = [polyValue p1 1] ++ (polyAsList p2)
polyAsList _           = []

polyNewton :: (Fractional a, Ord a) => Poly a -> a -> a
polyNewton p s = let s' = s - (polyValue p s)/(polyValue (polyDeriv p) s)
                     epsilon = 0.00001
                 in  if abs (s' - s) <= epsilon then s' else polyNewton p s'

{-
This function takes a list of integers and converts it to the recursive Poly definition
E.g., [1,2,3] -> 1*x^2 + 2*x + 3
-}
listToPoly :: Num a => [a] -> Poly a
listToPoly []     = Prod (Coef 0) X
listToPoly (c:cs) = Sum (Prod (Coef c) (termPower (length cs))) (listToPoly cs)
    where termPower p = if p == 0 then Coef 1
                        else Prod X (termPower (p - 1))

polyType :: Num a => Poly a -> a
polyType p = 0





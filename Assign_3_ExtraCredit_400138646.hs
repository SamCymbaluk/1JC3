-- Assignment 3 Extra Credit
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date Started 2017/10/21
-- Date Complete 2017/

module Polynomial where

import Test.QuickCheck

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

polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Sum p1 p2)  = Sum p1' p2'
    where p1' = polyDeriv p1
          p2' = polyDeriv p2
polyDeriv (Prod p1 p2) = Sum (Prod p1' p2) (Prod p2' p1)
    where p1' = polyDeriv p1
          p2' = polyDeriv p2
polyDeriv X            = Coef 1
polyDeriv (Coef _)     = Coef 0
{-
polyDeriv :: Num a => Poly a -> Poly a
polyDeriv (Sum p1 p2)       = Sum (polyDeriv p1) (polyDeriv p2)
polyDeriv (Prod (Coef c) p) = Prod (Coef c) (polyDeriv p)
polyDeriv (Prod X p)        = Prod (Coef (fromIntegral (polyDegree p + 1))) p
polyDeriv X                 = Coef 0
polyDeriv (Coef _)          = Coef 0
-}

polyAsList :: Num a => Poly a -> [a]
polyAsList (Sum p1 p2) = [polyValue p1 1] ++ (polyAsList p2)
polyAsList _           = []

polyNewton :: (Fractional a, Ord a) => Poly a -> a -> a
polyNewton p s = let s' = s - (polyValue p s)/(polyValue (polyDeriv p) s)
                     epsilon = 0.00001
                  in if abs (s' - s) <= epsilon then s' else polyNewton p s'

{-
This function takes a list of integers and converts it to the recursive Poly definition
E.g., [1,2,3] -> 1*x^2 + 2*x + 3
-}
listToPoly :: Num a => [a] -> Poly a
listToPoly []     = Prod (Coef 0) X
listToPoly (c:cs) = Sum (Prod (Coef c) (termPower (length cs))) (listToPoly cs)
    where termPower p = if p == 0 then Coef 1
                        else Prod X (termPower (p - 1))

polyToStr :: Show a => Poly a -> String
polyToStr X            = "x"
polyToStr (Coef a)     = show a
polyToStr (Prod p1 p2) = "("++(polyToStr p1)++")" ++ "*" ++ "("++(polyToStr p2)++")"
polyToStr (Sum p1 p2)  = "("++(polyToStr p1)++")" ++ "+" ++ "("++(polyToStr p2)++")"

{-polyIsConst :: Num a => Poly a -> Bool
polyIsConst p = let values = [polyValue p (pi*x) | x <- [1..5]]
                in False-}

polySimplify :: (Num a, Eq a) => Poly a -> Poly a
polySimplify (Sum (Coef a) (Coef b)) = Coef (a+b)
polySimplify (Prod (Coef a) (Coef b)) = Coef (a*b)
polySimplify (Prod p (Coef 0)) = Coef 0
polySimplify (Prod (Coef 0) p) = Coef 0
polySimplify (Sum p (Coef 0)) = polySimplify p
polySimplify (Sum (Coef 0) p) = polySimplify p
polySimplify (Prod p1 p2) = Prod (polySimplify p1) (polySimplify p2)
polySimplify (Sum p1 p2) = Sum (polySimplify p1) (polySimplify p2)
polySimplify p = p

{-
polyExpand :: Poly a -> Poly a
polyExpand (Prod p X) = Prod X p
polyExpand (Prod p (Sum s1 s2)) = Sum (Prod p s1) (Prod p s2)
polyExpand (Prod (Sum s1 s2) p) = Sum (Prod p s1) (Prod p s2)
polyExpand (Prod (Sum s1 s2) (Sum s3 s4)) = Sum (Sum (Prod s1 s2))
polyExpand (Sum p1 p2) =
polyExpand X = X
polyExpand (Coef a) = Coef a
polyExpand p = p
--}

polyDegree :: (Num a, Eq a) => Poly a -> Integer
polyDegree p = if degree >= 0 then degree else 0
               where degree = polyDegreeAux p 0

polyDegreeAux :: (Num a, Eq a) => Poly a -> Integer -> Integer
polyDegreeAux (Sum p1 p2) n      = n + (max (polyDegreeAux p1 0) (polyDegreeAux p2 0))
polyDegreeAux (Prod (Coef 0) p2) n = -1 -- If we multiply by zero,
polyDegreeAux (Prod p1 (Coef 0)) n = -1 -- ignore that portion of the Poly
polyDegreeAux (Prod p1 p2) n       = let -- If we get -1 as a degree, we know that we are multiplying by zero
    d1 = polyDegreeAux p1 0
    d2 = polyDegreeAux p2 0
   in if (d1 >= 0 && d2 >= 0) then n + d1 + d2 else -1
polyDegreeAux X n                  = n + 1
polyDegreeAux p n                  = n

poly = Prod (Prod (Sum X (Prod X (Prod X (Coef 4)))) (Sum X (Coef (-1)))) (Sum X (Coef (-2)))
--Sum (Prod X (Prod X (Sum X (Coef (-1))))) (Prod (Prod X (Sum X (Coef (-1)))) (Coef (-2)))
 -- Sum (Sum (Prod X (Prod X X)) (Prod X (Prod X (Coef (-1))))) (Sum (Prod (Coef (-2)) (Prod X X)) (Prod (Coef (-2)) (Prod X (Coef (-1)))))

very f x = (f . f . f . f . f) x

-- (x - 3)*(x + 2)*(5 - x**2)**2
poly2 = Prod (Sum X (Coef (-3))) (Prod (Sum X (Coef 2)) (Prod (Sum (Coef 5) (Prod (Coef (-1)) (Prod X X))) (Sum (Coef 5) (Prod (Coef (-1)) (Prod X X)))))

poly3 = Prod (Sum X (Coef (-3))) (Prod (Sum X (Coef 2)) (Prod (Sum (Coef 5) (Prod (Coef (-1)) (Prod X X))) (Sum (Coef 5) (Prod (Coef (-1)) X))))

poly4 = (Prod X (Prod (Prod X (Coef 0)) X))

poly5 = (Prod X (Prod X (Prod X (Prod X X))))

instance (Arbitrary a, Num a, Eq a, Ord a) => Arbitrary (Poly a) where
    arbitrary = do
        p
        return poly




polyDegreeProp1 :: (Num a, Eq a) => Poly a -> Bool
polyDegreeProp1 a = let
        d1 = polyDegree a
        d2 = polyDegree (polyDeriv a)
        d3 = polyDegree (polyDeriv (polyDeriv a))
    in (d2 + 1 == d1 || d2 == 0) && (d3 + 2 == d1 || d3 == 0)

-- Assignment 2
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/10/03

module VectorSpace where

type Vector = (Double, Double, Double)

vecZero :: Vector
vecZero = (0, 0, 0)

vecScalarProd :: Double -> Vector -> Vector
vecScalarProd x (a, b, c) = (x*a, x*b, x*c)

vecSum :: Vector -> Vector -> Vector
vecSum (a1, b1, c1) (a2, b2, c2) = (a1+a2, b1+b2, c1+c2)

vecMagnitude :: Vector -> Double
vecMagnitude (a, b, c) = sqrt (a**2 + b**2 + c**2)

vecF :: Vector -> [Vector] -> [Double]
vecF x ys =
    let xNeg = vecScalarProd (-1) x in
    [vecMagnitude (vecSum xNeg y) | y <- ys]



{-
Function:
Test Case Number:
Input:
Expected Output:
Actual Output:

vecScalerProd 0 x should return vecZero
-}
test1_vecScalarProd = vecScalarProd 0 (1,2,3) == vecZero
test2_vecScalarProd = vecScalarProd 5 (0,0,0) == vecZero
test3_vecScalarProd = let x = (1,(-1),5) in
    vecMagnitude (vecScalarProd 5 x) == 5 * vecMagnitude x
{-
-}

listConcat m n f
    | m > n  = []
    | m <= n = listConcat
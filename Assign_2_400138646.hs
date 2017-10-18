-- Assignment 2
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/10/03

module VectorSpace where

type Vector = (Double, Double, Double)
type Test = (String, Bool)

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


runTests :: IO ()
runTests = putStrLn (formatTests tests)
    where
    tests = [
        ("vecScalarProd Test 1", test1_vecScalarProd),
        ("vecScalarProd Test 2", test2_vecScalarProd),
        ("vecScalarProd Test 3", test3_vecScalarProd)]

formatTests :: [Test] -> String
formatTests [] = ""
formatTests ((name, fn):tests) = name ++ ": " ++ (if fn == True then "Passed" else "Failed") ++ "\n" ++ formatTests tests

{-
Function: vecScalarProd
Test Case Number: 1
Input: 0, (1,2,3)
Expected Output: vecZero
Actual Output: vecZero
-}
test1_vecScalarProd = vecScalarProd 0 (1,2,3) == vecZero
{-
Function: vecScalarProd
Test Case Number: 2
Input: 5, vecZero
Expected Output: vecZero
Actual Output: vecZero
-}
test2_vecScalarProd = vecScalarProd 5 vecZero == vecZero
{-
Function: vecScalarProd
Test Case Number: 3
Input: 5, (1, -1, 5)
Expected Output: ||output|| == 5 * ||(1, -1, 5)||
Actual Output: ||output|| == 5 * ||(1, -1, 5)||
-}
test3_vecScalarProd = let x = (1,(-1),5) in
    vecMagnitude (vecScalarProd 5 x) == 5 * vecMagnitude x

{-
Function:
Test Case Number:
Input:
Expected Output:
Actual Output:

vecScalarProd 0 x should return vecZero
-}


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
        ("vecScalarProd Test 3", test3_vecScalarProd),
        ("vecScalarProd Test 4", test4_vecScalarProd),
        ("vecSum Test 1", test1_vecSum),
        ("vecSum Test 2", test2_vecSum),
        ("vecSum Test 3", test3_vecSum),
        ("vecMagnitude Test 1", test1_vecMagnitude),
        ("vecMagnitude Test 2", test2_vecMagnitude),
        ("vecMagnitude Test 3", test3_vecMagnitude),
        ("vecF Test 1", test1_vecF),
        ("vecF Test 2", test2_vecF),
        ("vecF Test 3", test3_vecF)]

formatTests :: [Test] -> String
formatTests [] = ""
formatTests ((name, fn):tests) = name ++ ": " ++ (if fn == True then "Passed" else "Failed") ++ "\n" ++ formatTests tests

epsilon = 0.01 :: Double

test1_vecScalarProd = vecScalarProd 0 (1,2,3) == vecZero
test2_vecScalarProd = vecScalarProd 5 vecZero == vecZero
test3_vecScalarProd = let x = (1,(-1),5) in
    vecMagnitude (vecScalarProd 5 x) == 5 * vecMagnitude x
test4_vecScalarProd = let x = (1.0,(-1.0),5.0) in
    vecMagnitude (vecScalarProd 5.0 x) == 5.0 * vecMagnitude x

test1_vecSum = vecSum (1,2,3) vecZero == (1, 2, 3)
test2_vecSum = vecSum (1,2,3) (-1, -2, -3) == vecZero
test3_vecSum = vecSum (0.5, 1.0, 1.5) (5.5, -2.0, 10.0) == (6.0, -1.0, 11.5)

test1_vecMagnitude = vecMagnitude vecZero == 0
test2_vecMagnitude = abs (vecMagnitude (2.0, 2.0, 2.0) - sqrt 12) <= epsilon
test3_vecMagnitude = abs (vecMagnitude (-2.0, -2.0, -2.0) - sqrt 12) <= epsilon

test1_vecF = vecF vecZero [] == []
test2_vecF = let vecs = [(1.0, -1.0, 4.5), (5.6, 9.8, -4.5), (3.3, 5.6, 8.9)] in
    vecF vecZero vecs == [vecMagnitude x | x <- vecs]
test3_vecF = vecF (1.5, 5.0, 3.5) [(0.5, 3.0, 2.5)] == [sqrt 6]

{-
Function: vecScalarProd
Test Case Number: 1
Input: 0, (1,2,3)
Expected Output: vecZero
Actual Output: vecZero

Function: vecScalarProd
Test Case Number: 2
Input: 5, vecZero
Expected Output: vecZero
Actual Output: vecZero

Function: vecScalarProd
Test Case Number: 3
Input: 5, (1, -1, 5)
Expected Output: ||output|| == 5 * ||(1, -1, 5)||
Actual Output: ||output|| == 5 * ||(1, -1, 5)||

Function: vecScalarProd
Test Case Number: 4
Input: 5, (1.0, -1.0, 5.0)
Expected Output: ||output|| == 5.0 * ||(1.0, -1.0, 5.0)||
Actual Output: ||output|| == 5.0 * ||(1.0, -1.0, 5.0)||

Function: vecSum
Test Case Number: 1
Input: (1,2,3), vecZero
Expected Output: (1,2,3)
Actual Output: (1,2,3)

Function: vecSum
Test Case Number: 2
Input: (1,2,3), -(1,2,3)
Expected Output: vecZero
Actual Output: vecZero

Function: vecSum
Test Case Number: 3
Input: (0.5, 1.0, 1.5), (5.5, -2.0, 10.0)
Expected Output: (6.0, -1.0, 11.5)
Actual Output: (6.0, -1.0, 11.5)

Function: vecMagnitude
Test Case Number: 1
Input: vecZero
Expected Output: 0
Actual Output: 0

Function: vecMagnitude
Test Case Number: 2
Input: (2.0, 2.0, 2.0)
Expected Output: sqrt 12
Actual Output: sqrt 12

Function: vecMagnitude
Test Case Number: 3
Input: (-2.0, -2.0, -2.0)
Expected Output: sqrt 12
Actual Output: sqrt 12

Function: vecF
Test Case Number: 1
Input: vecZero, []
Expected Output: []
Actual Output: []

Function: vecF
Test Case Number: 2
Input: vecZero, [a, b, c]
Expected Output: [||a||, ||b||, ||c||]
Actual Output: [||a||, ||b||, ||c||]

Function: vecF
Test Case Number: 3
Input: (1.5, 5.0, 3.5), [(0.5, 3.0, 2.5)]
Expected Output: [sqrt 6]
Actual Output: [sqrt 6]
-}
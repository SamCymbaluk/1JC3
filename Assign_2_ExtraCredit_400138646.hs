-- Assignment 2 Extra Credit
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/10/03

module VectorSpace where

type Test = (String, Bool)

newtype Vector2 a = Vector2 (a, a)
    deriving (Show, Eq)
newtype Vector3 a = Vector3 (a, a, a)
    deriving (Show, Eq)
newtype Vector4 a = Vector4 (a, a, a, a)
    deriving (Show, Eq)


class VectorSpace v where
    vecZero :: (Num a) => v a
    vecSum :: (Num a) => v a -> v a -> v a
    vecScalarProd :: (Num a) => a -> v a -> v a
    vecMagnitude :: (Floating a) => v a -> a

    vecF :: (Floating a) => v a -> [v a] -> [a]
    vecF x ys =
        let xNeg = vecScalarProd (-1) x in
        [vecMagnitude (vecSum xNeg y) | y <- ys]

instance VectorSpace Vector2 where
    vecZero = Vector2 (0, 0)
    vecSum (Vector2 (a1, b1)) (Vector2 (a2, b2)) =
        Vector2 (a1+a2, b1+b2)
    vecScalarProd x (Vector2 (a, b)) =
        Vector2 (x*a, x*b)
    vecMagnitude (Vector2 (a, b)) =
        sqrt(a**2 + b**2)

instance VectorSpace Vector3 where
    vecZero = Vector3 (0, 0, 0)
    vecSum (Vector3 (a1, b1, c1)) (Vector3 (a2, b2, c2)) =
        Vector3 (a1+a2, b1+b2, c1+c2)
    vecScalarProd x (Vector3 (a, b, c)) =
        Vector3 (x*a, x*b, x*c)
    vecMagnitude (Vector3 (a, b, c)) =
        sqrt(a**2 + b**2 + c**2)

instance VectorSpace Vector4 where
    vecZero = Vector4 (0, 0, 0, 0)
    vecSum (Vector4 (a1, b1, c1, d1)) (Vector4 (a2, b2, c2, d2)) =
        Vector4 (a1+a2, b1+b2, c1+c2, d1+d2)
    vecScalarProd x (Vector4 (a, b, c, d)) =
        Vector4 (x*a, x*b, x*c, x*d)
    vecMagnitude (Vector4 (a, b, c, d)) =
        sqrt(a**2 + b**2 + c**2 + d**2)

runTests :: IO ()
runTests = putStrLn (formatTests tests)
    where
    tests = [
        ("vecScalarProd Test 1", test1_vecScalarProd),
        ("vecScalarProd Test 2", test2_vecScalarProd),
        ("vecScalarProd Test 3", test3_vecScalarProd),
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

test1_vecScalarProd = let x = Vector2 (1.0,(-1.0)) in
    vecMagnitude (vecScalarProd 5.0 x) == 5.0 * vecMagnitude x
test2_vecScalarProd = let x = Vector3 (1.0,(-1.0), 5.0) in
    vecMagnitude (vecScalarProd 5.0 x) == 5.0 * vecMagnitude x
test3_vecScalarProd = let x = Vector4 (1.0,(-1.0), 5.0, 11.0) in
    vecMagnitude (vecScalarProd 5.0 x) == 5.0 * vecMagnitude x

test1_vecSum = vecSum (Vector2 (0.5, 1.0)) (Vector2 (5.5, -2.0)) == Vector2 (6.0, -1.0)
test2_vecSum = vecSum (Vector3 (0.5, 1.0, 1.5)) (Vector3 (5.5, -2.0, 10.0)) == Vector3 (6.0, -1.0, 11.5)
test3_vecSum = vecSum (Vector4 (0.5, 1.0, 1.5, -5.0)) (Vector4 (5.5, -2.0, 10.0, 0.0)) == Vector4 (6.0, -1.0, 11.5, -5.0)

test1_vecMagnitude = abs (vecMagnitude (Vector2 (2.0, 2.0)) - sqrt 8) <= epsilon
test2_vecMagnitude = abs (vecMagnitude (Vector3 (2.0, 2.0, 2.0)) - sqrt 12) <= epsilon
test3_vecMagnitude = abs (vecMagnitude (Vector4 (2.0, 2.0, 2.0, 2.0)) - 4.0) <= epsilon


test1_vecF = vecF (Vector2 (1.5, 5.0)) [Vector2 (0.5, 3.0)] == [sqrt 5]
test2_vecF = vecF (Vector3 (1.5, 5.0, 3.5)) [Vector3 (0.5, 3.0, 2.5)] == [sqrt 6]
test3_vecF = vecF (Vector4 (1.5, 5.0, 3.5, -5.3)) [Vector4 (0.5, 3.0, 2.5, 0.7)] == [sqrt 42]

{-
Function: vecScalarProd
Test Case Number: 1
Input: 5, (1.0, -1.0)
Expected Output: ||output|| == 5.0 * ||(1.0, -1.0)||
Actual Output: ||output|| == 5.0 * ||(1.0, -1.0)||

Function: vecScalarProd
Test Case Number: 2
Input: 5, (1.0, -1.0, 5.0)
Expected Output: ||output|| == 5.0 * ||(1.0, -1.0, 5.0)||
Actual Output: ||output|| == 5.0 * ||(1.0, -1.0, 5.0)||

Function: vecScalarProd
Test Case Number: 3
Input: 5, (1.0, -1.0, 5.0, 11.0)
Expected Output: ||output|| == 5.0 * ||(1.0, -1.0, 5.0, 11.0)||
Actual Output: ||output|| == 5.0 * ||(1.0, -1.0, 5.0, 11.0)||

Function: vecSum
Test Case Number: 1
Input: (0.5, 1.0), (5.5, -2.0)
Expected Output: (6.0, -1.0)
Actual Output: (6.0, -1.0)

Function: vecSum
Test Case Number: 2
Input: (0.5, 1.0, 1.5), (5.5, -2.0, 10.0)
Expected Output: (6.0, -1.0, 11.5)
Actual Output: (6.0, -1.0, 11.5)

Function: vecSum
Test Case Number: 3
Input: (0.5, 1.0, 1.5, -5.0), (5.5, -2.0, 10.0, 0.0)
Expected Output: (6.0, -1.0, 11.5, -5.0)
Actual Output: (6.0, -1.0, 11.5, -5.0)

Function: vecMagnitude
Test Case Number: 1
Input: (2.0, 2.0)
Expected Output: sqrt 8
Actual Output: sqrt 8

Function: vecMagnitude
Test Case Number: 2
Input: (2.0, 2.0, 2.0)
Expected Output: sqrt 12
Actual Output: sqrt 12

Function: vecMagnitude
Test Case Number: 3
Input: (2.0, 2.0, 2.0, 2.0)
Expected Output: 4.0
Actual Output: 4.0

Function: vecF
Test Case Number: 1
Input: (1.5, 5.0), [(0.5, 3.0)]
Expected Output: [sqrt 5]
Actual Output: [sqrt 5]

Function: vecF
Test Case Number: 2
Input: (1.5, 5.0, 3.5), [(0.5, 3.0, 2.5)]
Expected Output: [sqrt 6]
Actual Output: [sqrt 6]

Function: vecF
Test Case Number: 3
Input: (1.5, 5.0, 3.5, -5.3), [(0.5, 3.0, 2.5, 0.7)]
Expected Output: [sqrt 42]
Actual Output: [sqrt 42]
-}



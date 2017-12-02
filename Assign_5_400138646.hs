-- Assignment 5
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/11/18

module DefiniteIntegral where

import Test.QuickCheck

{-
Approximates the definite integral of a function on the interval [a, b]
Uses the trapezoid rule to calculate the approximation
Accuracy (and necessary computations) can be varied by 'n'
-}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n = let
    dx     = (b - a) / (fromInteger n)
    x i    = a + (dx * i) -- x i varies from a to b by some amount dx
    area i = ((g (x (i - 1)) +  g (x i)) / 2)*dx
   in sigmaSum area 1 n

{-
Takes the sigma sum of a function
-}
sigmaSum :: Num n => (n -> n) -> Integer -> Integer -> n
sigmaSum f a b
    | a <= b    =  f (fromInteger a) + sigmaSum f (fromInteger a + 1) b
    | otherwise = 0

{-
Approximates the area of a circle by integrating over small
concentric slices that are approximated as rectangles
-}
circleArea :: Double -> Double
circleArea r = definiteIntegral 0 r (2*pi*) 1000

{-
Approximates the volume of a sphere by integrating over A(y(x))
where a(x) is the area of a circle of radius r
      y(x) is the radius of a sphere at point x along its diameter
-}
sphereVolume :: Double -> Double
sphereVolume r = let
    a x = circleArea (y x)
    y x = sqrt(r^2 - (x - r)^2) -- Gives the radius of the sphere (y) at a value x
   in definiteIntegral 0 (2 * r) a 1000

doubleEq :: Double -> Double -> Bool
d1 `doubleEq` d2 = let
    epsilon = abs (d1 * 0.01)
   in abs (d1 - d2) <= (max epsilon 0.01)

{-
Evaluates the list representation of a polynomials using Horner's method
-}
polyListValue :: [Double] -> Double -> Double
polyListValue (x:xs) n = x + n*(polyListValue xs n)
polyListValue [] n     = 0

{-
Function: definiteIntegral
Property: The integral from a to b = negative the integral from b to a
Actual Test Result: Pass
-}
definiteIntegralProp1 :: (Double, Double, [Double]) -> Bool
definiteIntegralProp1 (a, b, poly) = let
    f = polyListValue poly
    intAB = definiteIntegral a b f 10000
    intBA = definiteIntegral b a f 10000
   in intAB `doubleEq` (-intBA)

{-
Function: definiteIntegral
Property: The integral of a constant over an interval = constant * interval
Actual Test Result: Pass
-}
definiteIntegralProp2 :: (Double, Double, Double) -> Bool
definiteIntegralProp2 (a, b, c) = let
    interval = b - a
    intAB = definiteIntegral a b (\x -> c) 10000
   in intAB `doubleEq` (interval * c)

{-
Function: definiteIntegral
Property: The integral from a to a = 0
Actual Test Result: Pass
-}
definiteIntegralProp3 :: (Double, [Double]) -> Bool
definiteIntegralProp3 (a, poly) = let
    f = polyListValue poly
   in (definiteIntegral a a f 10000) == 0

{-
Test Plan

Function: definiteIntegral
Test Case Number: 1
Input: 0 5 (\x -> 3*x^2) 1000
Expected Output: 125
Actual Output: 125.00006250000015

Function: definiteIntegral
Test Case Number: 2
Input: 0 (-5) (\x -> 5) 1000
Expected Output: -25
Actual Output: -24.99999999999965

Function: definiteIntegral
Test Case Number: 3
Input: 0 (5) (\x -> x*exp(x)) 1000
Expected Output: exp(5)*(4) + 1 ~= 593.7
Actual Output: 593.6526364103064

Function: circleArea
Test Case Number: 1
Input: 0
Expected Output: 0
Actual Output: 0.0

Function: circleArea
Test Case Number: 2
Input: sqrt(1/pi)
Expected Output: 1
Actual Output: 1.0000000000000002

Function: circleArea
Test Case Number: 3
Input: 5
Expected Output: 25*pi ~= 78.5
Actual Output: 78.53981633974482

Function: sphereVolume
Test Case Number: 1
Input: 0
Expected Output: 0
Actual Output: 0.0

Function: sphereVolume
Test Case Number: 2
Input: 1
Expected Output: (4/3)*pi ~=1.89
Actual Output: 4.1887860159961825

Function: sphereVolume
Test Case Number: 3
Input: 5
Expected Output: (4/3)*pi*125 ~= 523.6
Actual Output: 523.5982519995226
-}

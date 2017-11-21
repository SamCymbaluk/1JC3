-- Assignment 5
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/11/18

module DefiniteIntegral where

import Test.QuickCheck
import Test.QuickCheck.Function

definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n = let
    dx = (b - a) / (fromInteger n)
    x i = a + (dx * i)
    area = \i -> ((g (x (i - 1)) +  g (x i)) / 2)*dx
   in sigmaSum area 1 n

sigmaSum :: Num n => (n -> n) -> Integer -> Integer -> n
sigmaSum f a b
    | a <= b    =  f (fromInteger a) + sigmaSum f (fromInteger a + 1) b
    | otherwise = 0

circleArea :: Double -> Double
circleArea r = definiteIntegral 0 r (\x -> 2*pi*x) 1000

sphereVolume :: Double -> Double
sphereVolume r = let
    y x = sqrt(r^2 - (x - r)^2)
   in definiteIntegral 0 (2 * r) (\x -> pi * (y x)^2) 1000


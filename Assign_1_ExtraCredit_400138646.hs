-- Assignment 1 Extra Credit
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/09/28
import Data.Complex

cbrt :: Float -> Float
cbrt x = if x >= 0 then x**(1/3) else -abs (x)**(1/3)

complexCbrt :: Complex Float -> Complex Float
complexCbrt x = x**(1/3)

cubicQ :: Float -> Float -> Float -> Float
cubicQ a b c = (3 * a * c - b**2) / (9 * a**2)

cubicR :: Float -> Float -> Float -> Float -> Float
cubicR a b c d = (9 * a * b * c - 27 * a**2 * d - 2 * b**3) / (54 * a**3)

cubicS :: Float -> Float -> Float
cubicS q r = cbrt (r + sqrt(q**3 + r**2))

cubicT :: Float -> Float -> Float
cubicT q r = cbrt (r - sqrt(q**3 + r**2))

cubicComplexS :: Float -> Float -> Complex Float
cubicComplexS q r =
    if discriminant q r < 0 then complexCbrt((r :+ 0) + sqrt((q**3 + r**2) :+ 0))
    else ((cubicS q r) :+ 0)

cubicComplexT :: Float -> Float -> Complex Float
cubicComplexT q r =
    if discriminant q r < 0 then complexCbrt((r :+ 0) - sqrt((q**3 + r**2) :+ 0))
    else ((cubicT q r) :+ 0)

discriminant :: Float -> Float -> Float
discriminant q r = q**3 + r**2

cubicRealSolution :: Float -> Float -> Float -> Float -> Float
cubicRealSolution a b c d =
    let q = cubicQ a b c
        r = cubicR a b c d
        s = cubicComplexS q r
        t = cubicComplexT q r
    in  realPart (s + t) - (b / (3 * a))

check :: Float -> Float -> Float -> Float -> Float -> Bool
check a b c d x =
    let epsilon = 0.01
    in abs(a*x**3 + b*x**2 + c*x + d) <=epsilon

testCheck :: [(Float, Float, Float, Float, Float, Bool)]
testCheck = [(a, b, c, d, x, result) |
    (a, b, c, d, x, result) <- [(a, b, c, d, (cubicRealSolution a b c d), check a b c d (cubicRealSolution a b c d))
    | a <- [(-5)..5], b <- [(-5)..5], c <- [(-5)..5], d <- [(-5)..5], a /= 0], result == False]

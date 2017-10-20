-- Assignment 2 Extra Credit
-- Author: Sam Cymbaluk
-- ID: 400138646
-- Date 2017/10/03

module VectorSpace where

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





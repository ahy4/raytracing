{-# LANGUAGE NoImplicitPrelude #-}

module Vec3 where

import Prelude

data Vec3 a = Vec3 a a a deriving (Show, Read, Eq)
-- data Point = Point Float Float deriving (Show)

type Pick a = Vec3 a -> a
type Vec3Operator a = Vec3 a -> Vec3 a -> Vec3 a
type Vec3SingleOperator a = Vec3 a -> Vec3 a

x :: Pick Float
x (Vec3 x _ _) = x
--
y :: Pick Float
y (Vec3 _ y _) = y

z :: Pick a
z (Vec3 _ _ z) = z

r :: Pick Float
g :: Pick Float
b :: Pick Float
r = x
g = y
b = z

(+:) :: Vec3Operator Float
(Vec3 x1 y1 z1) +: (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = x1 + x2
    y3 = y1 + y2
    z3 = z1 + z2

(-:) :: Vec3Operator Float
(Vec3 x1 y1 z1) -: (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = x1 - x2
    y3 = y1 - y2
    z3 = z1 - z2

(*:) :: Vec3Operator Float
(Vec3 x1 y1 z1) *: (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = x1 * x2
    y3 = y1 * y2
    z3 = z1 * z2

(/:) :: Vec3Operator Float
(Vec3 x1 y1 z1) /: (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = x1 / x2
    y3 = y1 / y2
    z3 = z1 / z2

(.:) :: Float -> Vec3 Float -> Vec3 Float
t .: (Vec3 x y z) = Vec3 (t * x) (t * y) (t * z)

cross :: Vec3Operator Float
(Vec3 x1 y1 z1) `cross` (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = (y1 * z2) - (z1 * y2)
    y3 = (y1 * x2) - (x1 * y2)
    z3 = (x1 * y2) - (y1 * x2)

length :: Pick Float
length = sqrt . squaredLength

squaredLength :: Pick Float
squaredLength (Vec3 x y z) = sum [x ** 2, y ** 2, z ** 2]

unitVector :: Vec3 Float -> Vec3 Float
unitVector v = (1 / Vec3.length v) .: v

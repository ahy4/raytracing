module Vec3 where

import Prelude
import GHC.Float

data Vec3 = Vec3 {
  x :: Float,
  y :: Float,
  z :: Float
} deriving (Show, Read, Eq)

type Vec3Operator = Vec3 -> Vec3 -> Vec3
type Vec3SingleOperator = Vec3 -> Vec3

r :: Vec3 -> Float
g :: Vec3 -> Float
b :: Vec3 -> Float
r = x
g = y
b = z

(+:) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) +: (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = x1 + x2
    y3 = y1 + y2
    z3 = z1 + z2

(-:) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) -: (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = x1 - x2
    y3 = y1 - y2
    z3 = z1 - z2

(*:) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) *: (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = x1 * x2
    y3 = y1 * y2
    z3 = z1 * z2

(/:) :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) /: (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = x1 / x2
    y3 = y1 / y2
    z3 = z1 / z2

(.:) :: Vec3 -> Vec3 -> Float
v1 .: v2 = x v + y v + z v
  where
    v = v1 *: v2

scale :: Float -> Vec3 -> Vec3
scale t v = Vec3 t t t *: v

cross :: Vec3 -> Vec3 -> Vec3
(Vec3 x1 y1 z1) `cross` (Vec3 x2 y2 z2) = Vec3 x3 y3 z3
  where
    x3 = (y1 * z2) - (z1 * y2)
    y3 = (z1 * x2) - (x1 * z2)
    z3 = (x1 * y2) - (y1 * x2)

length :: Vec3 -> Float
length = sqrt . squaredLength

squaredLength :: Vec3 -> Float
squaredLength v = v .: v

unitVector :: Vec3 -> Vec3
unitVector v = scale (1 / Vec3.length v) v

average :: [Vec3] -> Vec3
average vectors = scale (1 / int2Float (Prelude.length vectors)) $ foldl1 (+:) vectors

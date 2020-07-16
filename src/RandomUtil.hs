module RandomUtil where

import Prelude
import System.Random
import Vec3

-- -1 < x,y,z < +1 における、単位球内の点を選ぶ
randomInUnitSphere :: StdGen -> (StdGen, Vec3)
randomInUnitSphere g1
  | squaredLength p <= 1 = (g4, p)
  | otherwise = randomInUnitSphere g4
  where
    p = scale 2 (Vec3 r1 r2 r3) -: Vec3 1 1 1
    (r1, g2) = random g1
    (r2, g3) = random g2
    (r3, g4) = random g3

createRandomGeneratorsLazy :: RandomGen g => g -> [g]
createRandomGeneratorsLazy g = g1:gs
  where
    (g1, g2) = split g
    gs = createRandomGeneratorsLazy g2

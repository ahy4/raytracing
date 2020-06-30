module Lib where

import Data.List
import Data.Maybe
import Vec3
import Ray
import Prelude
import Hitable
import HitableList
import Sphere
import Camera
import System.Random
import Control.Monad
import GHC.Float
import Debug.Trace

main :: IO ()
main = writeFile "./dest/image.ppm" =<< ppmText <$> newStdGen

width :: Int
width = 200

height :: Int
height = 100

spheres =
  [
    Sphere {
      center=Vec3 0 0 (-1),
      radius=0.5
    },
    Sphere {
      center=Vec3 0 (-100.5) (-1),
      radius=100
    }
  ]

world = HitableList spheres

-- -1 < x,y,z < +1 における、単位球内の点を選ぶ
randomInUnitSphere :: StdGen -> Vec3
randomInUnitSphere g1 = Vec3 1 1 1
  where
    p = scale 2
    (r1, g2) = next g1
    (r2, g3) = next g2
    (r3, g4) = next g3
    (r4,  _) = next g4

color :: Hitable a => Ray -> a -> Vec3
color ray hitable
  | isHit     = scale 0.5 $ normal (fromJust hitResult) +: Vec3 1 1 1
  | otherwise = gradation t (Vec3 1 1 1) (Vec3 0.5 0.7 1)
  where
    unitDirection = unitVector $ direction ray
    t = (*) 0.5 $ y unitDirection + 1.0
    hitResult = hit hitable ray 0 1000000
    isHit = isJust hitResult

gradation :: Float -> Vec3 -> Vec3 -> Vec3
gradation t from to = scale (1.0 - t) from +: scale t to

ppmText :: StdGen -> String
ppmText gen = header ++ body ++ "\n"
  where
    header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body :: String
    body = intercalate "\n" [ toRgbText $ antialias 10 colorFn (x, y) |
      y <- reverse [0..int2Float height-1],
      x <- [0..int2Float width-1] ]
    colorFn (x, y) = color (getRay x y) world

antialias :: Int -> ((Float, Float) -> Vec3) -> (Float, Float) -> Vec3
antialias len originalFunc appliedPoint = average colors
  where
    colors :: [Vec3]
    colors = map (aroundColor appliedPoint) distances
    aroundColor :: (Float, Float) -> (Float, Float) -> Vec3
    aroundColor (x, y) (dx, dy) = originalFunc ((x + dx) / int2Float width, (y + dy) / int2Float height)
    l = int2Float len
    distances = [ (x/l, y/l) | x <- [0..l-1], y <- [0..l-1]]

toRgbText :: Vec3 -> String
toRgbText (Vec3 r g b) = unwords [scale r, scale g, scale b]
  where
    scale = show . floor . (*) 255.99

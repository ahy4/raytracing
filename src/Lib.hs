module Lib (main) where

import Data.List
import Data.Maybe
import Vec3
import Ray (Ray(..), pointAtParameter)
import Prelude
import Hitable
import HitableList
import Sphere
import Camera
import System.Random
import Control.Monad
import GHC.Float

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
  | otherwise = scale (1.0 - t) (Vec3 1 1 1) +: scale t (Vec3 0.5 0.7 1)
  where
    unitDirection = unitVector $ direction ray
    t = (*) 0.5 $ y unitDirection + 1.0
    hitResult = hit hitable ray 0 1000000
    isHit = isJust hitResult

ppmText :: StdGen -> String
ppmText gen = header ++ body ++ "\n"
  where
    header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body :: String
    body = foldl1 glue
      $ sequence [
        toRgbText $ antialias colorFn (x, y) |
        y <- reverse [0..int2Float height-1],
        x <- [0..int2Float width-1]
      ]
    colorFn (x, y) = color (getRay x y) world
    glue a b = a ++ "\n" ++ b

antialias :: ((Float, Float) -> Vec3) -> (Float, Float) -> Vec3
antialias f p = average colors
  where
    colors :: [Vec3]
    colors = map (aroundColor p) [(0,0)]
    aroundColor :: (Float, Float) -> (Float, Float) -> Vec3
    aroundColor (x, y) (dx, dy) = f ((x + dx) / int2Float width, (y + dy) / int2Float height)
    n = 10.0
    distances = [ (x/n, y/n) | x <- [0..n-1], y <- [0..n-1]]

toRgbText :: Vec3 -> String
toRgbText (Vec3 r g b) = unwords [scale r, scale g, scale b]
  where
    scale = show . floor . (*) 255.99

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
import Util
import Control.Monad

main :: IO ()
main = writeFile "./dest/image.ppm" =<< ppmText

width :: Int
width = 200

height :: Int
height = 100

ns = 100

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

color :: Hitable a => Ray -> a -> Vec3
color ray hitable
  | isHit     = scale 0.5 $ normal (fromJust hitResult) +: Vec3 1 1 1
  | otherwise = scale (1.0 - t) (Vec3 1 1 1) +: scale t (Vec3 0.5 0.7 1)
  where
    unitDirection = unitVector $ direction ray
    t = (*) 0.5 $ y unitDirection + 1.0
    hitResult = hit hitable ray 0 1000000
    isHit = isJust hitResult

ppmText :: IO String
ppmText = (header ++) . (++ "\n") <$> body
  where
    header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body :: IO String
    body = foldl1 glue
      <$> sequence [ 
        toRgbText . antialias colorFn (x, y) <$> newStdGen |
        y <- reverse [ 0 .. previous height ],
        x <- [ 0 .. previous width]
      ]
    previous = (+) (-1) <$> toFloat
    colorFn (x, y) = color (getRay x y) world
    glue a b = a ++ "\n" ++ b

antialias :: ((Float, Float) -> Vec3) -> (Float, Float) -> StdGen -> Vec3
antialias f p stdGen = average colors
  where
    colors :: [Vec3]
    colors = map (aroundColor p) zipped
    aroundColor :: (Float, Float) -> (Float, Float) -> Vec3
    aroundColor (x, y) (dx, dy) = f ((x + dx) / toFloat width, (y + dy) / toFloat height)
    createRandomList :: StdGen -> [Float]
    createRandomList stgGen = take ns $ randomRs (0, 1) stgGen :: [Float]
    zipped = zip (createRandomList stdGen) $ createRandomList $ snd $ next stdGen

toRgbText :: Vec3 -> String
toRgbText (Vec3 r g b) = unwords [scale r, scale g, scale b]
  where
    scale = show . floor . (*) 255.99

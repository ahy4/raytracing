module Lib where

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

someFunc :: IO ()
someFunc = writeFile "./aaa.ppm" =<< ppmText

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

color :: Hitable a => Ray -> a -> Vec3 Float
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
        toRgbText <$> antialias colorFn (x, y) |
        y <- reverse [ 0 .. previous height ],
        x <- [ 0 .. previous width]
      ]
    previous = (+) (-1) <$> toFloat
    colorFn (x, y) = color (getRay x y) world
    glue a b = a ++ "\n" ++ b

antialias :: ((Float, Float) -> Vec3 Float) -> (Float, Float) -> IO (Vec3 Float)
antialias f p = average <$> colors
  where
    zipped :: IO [(Float, Float)]
    zipped = zip <$> createRandomList ns <*> createRandomList ns
    colors :: IO [Vec3 Float]
    colors = map (aroundColor p) <$> zipped
    aroundColor :: (Float, Float) -> (Float, Float) -> Vec3 Float
    aroundColor (x, y) (dx, dy) = f ((x + dx) / toFloat width, (y + dy) / toFloat height)

createRandomList :: Int -> IO [Float]
createRandomList len = replicateM len $ randomRIO (0, 1::Float)
-- createRandomList len = sequence $ take len $ repeat $ randomRIO (0, 1::Float)

toRgbText :: Vec3 Float -> String
toRgbText (Vec3 r g b) = unwords [scale r, scale g, scale b]
  where
    scale = show . floor . (*) 255.99

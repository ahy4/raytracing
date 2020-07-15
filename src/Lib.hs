{-# LANGUAGE RankNTypes #-}

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
import Color
import System.Random
import Control.Monad
import Control.Exception
import GHC.Float
import Debug.Trace
import RandomUtil

main :: IO ()
main = writeFileWrap "./dest/image.ppm" =<< text
  where
    text :: IO (Maybe String)
    text = ppmText <$> newStdGen
    writeFileWrap :: String -> Maybe String -> IO ()
    writeFileWrap path content
      | isJust content = writeFile path $ fromJust content
      | otherwise = fail "Second argument is not Just."

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

color :: Hitable a => Ray -> a -> StdGen -> Maybe Color
color ray hitable gen
  | isHit     = scaleMaybeColor 0.5 $ color nextRay world nextGen
  | otherwise = mkColor $ gradation t (Vec3 1 1 1) (Vec3 0.5 0.7 1)
  where
    isHit = isJust hitResult
    hitResult = hit hitable ray 1e-10 1e10 -- when t=0, hit self object

    -- when hit
    hitRecord = fromJust hitResult
    (nextGen, random) = randomInUnitSphere gen
    nextRay = Ray {
        origin = p hitRecord,
        direction = normal hitRecord +: random
      }
    scaleMaybeColor :: Float -> Maybe Color -> Maybe Color
    scaleMaybeColor _ Nothing = Nothing
    scaleMaybeColor x (Just color) = mkColor $ scale x $ props color

    -- when not hit
    gradation :: Float -> Vec3 -> Vec3 -> Vec3
    gradation t from to = scale (1.0 - t) from +: scale t to
    unitDirection = unitVector $ direction ray
    t = (*) 0.5 $ y unitDirection + 1.0

ppmText :: StdGen -> Maybe String
ppmText gen 
  | hasNoErr = Just $ header ++ body ++ "\n"
  | otherwise = Nothing
  where
    header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body = intercalate "\n" $ map (toRgbText.fromJust) colors
    hasNoErr = all isJust colors
    gens = createRandomGeneratorsLazy gen
    positions = [ (x, y) |
      y <- reverse [0..int2Float height-1],
      x <- [0..int2Float width-1] ]
    zipped = zip positions gens
    colorFn (x, y) = color (getRay x y) world
    colors = [ antialias 10 colorFn pos g | (pos, g) <- zipped ]

type ColorFunctionType = (Float, Float) -> StdGen -> Maybe Color

antialias :: Int -> ColorFunctionType -> ColorFunctionType
antialias len originalFunc appliedPoint gen = avr colors
  where
    avr :: [Maybe Color] -> Maybe Color
    avr xs
      | all isJust xs = mkColor $ average $ map (props.fromJust) xs
      | otherwise = Nothing
    colors :: [Maybe Color]
    colors = map (uncurry $ aroundColor appliedPoint) zipped
    aroundColor :: (Float, Float) -> (Float, Float) -> StdGen-> Maybe Color
    aroundColor (x, y) (dx, dy) = originalFunc pos
      where
        pos = ((x + dx) / int2Float width, (y + dy) / int2Float height)
    l = int2Float len
    gens = createRandomGeneratorsLazy gen
    distances = [ (x/l, y/l) | x <- [0..l-1], y <- [0..l-1] ]
    zipped = zip distances gens

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

color :: Hitable a => Ray -> a -> StdGen -> Maybe Color
color ray hitable gen
  | isHit     = mkColor $ scale 0.5 $ normal (fromJust hitResult) +: Vec3 1 1 1
  | otherwise = mkColor $ gradation t (Vec3 1 1 1) (Vec3 0.5 0.7 1)
  where
    unitDirection = unitVector $ direction ray
    t = (*) 0.5 $ y unitDirection + 1.0
    hitResult = hit hitable ray 0 1e10
    isHit = isJust hitResult

gradation :: Float -> Vec3 -> Vec3 -> Vec3
gradation t from to = scale (1.0 - t) from +: scale t to

ppmText :: StdGen -> Maybe String
ppmText gen 
  | hasNoErr = Just $ header ++ body ++ "\n"
  | otherwise = Nothing
  where
    header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body = intercalate "\n" $ map (toRgbText.fromJust) colors
    hasNoErr = all isJust colors
    colors = [ antialias 10 colorFn (x, y) |
      y <- reverse [0..int2Float height-1],
      x <- [0..int2Float width-1] ]
    colorFn (x, y) = color (getRay x y) world gen

type ColorFunctionType = (Float, Float) -> Maybe Color

antialias :: Int -> ColorFunctionType -> ColorFunctionType
antialias len originalFunc appliedPoint = avr colors
  where
    avr :: [Maybe Color] -> Maybe Color
    avr xs
      | all isJust xs = mkColor $ average $ map (props.fromJust) xs
      | otherwise = Nothing
    colors :: [Maybe Color]
    colors = map (aroundColor appliedPoint) distances
    aroundColor :: (Float, Float) -> (Float, Float) -> Maybe Color
    aroundColor (x, y) (dx, dy) = originalFunc ((x + dx) / int2Float width, (y + dy) / int2Float height)
    l = int2Float len
    distances = [ (x/l, y/l) | x <- [0..l-1], y <- [0..l-1]]

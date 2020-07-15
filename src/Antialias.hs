module Antialias (ColorFunctionType, antialias) where

import System.Random
import Prelude
import Color
import Data.Maybe
import Vec3
import GHC.Float
import RandomUtil

type ColorFunctionType = (Float, Float) -> StdGen -> Maybe Color

-- どうしよう
height=100
width=200

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

module Antialias (ColorFunctionType, antialias) where

import System.Random
import Prelude
import Color
import Data.Maybe
import Vec3
import GHC.Float
import RandomUtil
import Debug.Trace

type ColorFunctionType = (Float, Float) -> StdGen -> Maybe Color

antialias :: Int -> (Float, Float) -> ColorFunctionType -> ColorFunctionType
-- 1pxのサイズ/スケールがわからないので、widthとheightをもらう必要がある
antialias len (canvasWidth, canvasHeight) originalFunc appliedPoint gen = avr colors
  where
    avr :: [Maybe Color] -> Maybe Color
    avr xs
      | all isJust xs = mkColor $ average $ map (props.fromJust) xs
      | otherwise = Nothing
    colors :: [Maybe Color]
    colors = map (uncurry $ aroundColor appliedPoint) zipped
    aroundColor :: (Float, Float) -> (Float, Float) -> StdGen-> Maybe Color
    aroundColor (u, v) (du, dv) = originalFunc (u + du, v + dv)
    l = int2Float len
    gens = createRandomGeneratorsLazy gen
    distances = [ (du/l/canvasWidth, dv/l/canvasHeight) | du <- [0..l-1], dv <- [0..l-1] ]
    zipped = zip distances gens

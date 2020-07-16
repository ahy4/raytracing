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

antialias :: Int -> ColorFunctionType -> ColorFunctionType
-- どのくらい周辺をチェックしてantialiasの参考にするかを決めないとなので、duのサイズかwidthを渡す必要がある
antialias len originalFunc appliedPoint gen = avr colors
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
    distances = [ (du/l, dv/l) | du <- [0..l-1], dv <- [0..l-1] ]
    zipped = zip distances gens

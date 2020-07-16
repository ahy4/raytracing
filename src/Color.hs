module Color where

import Prelude
import Vec3
import Debug.Trace
import Data.Maybe

newtype Color = UnsafeMkColor { props :: Vec3 } deriving (Show, Read, Eq)

mkColor :: Vec3 -> Maybe Color
mkColor vec
  | all inRange elms = Just $ UnsafeMkColor vec
  | otherwise = Nothing
  where
    inRange val = 0 <= val && val <= 1
    elms = [x, y, z] <*> [vec]

toRgbText :: Color -> String
toRgbText color = unwords [createBy x, createBy y, createBy z]
  where
    -- sqrtはガンマ補正2
    createText = show . floor . (*) 255.99 . sqrt
    createBy getter = createText $ getter $ props color

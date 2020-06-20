module Hitable where

import Prelude (Float, Bool, ($), Show, Eq, Maybe)
import Vec3
import Ray

data HitRecord = HitRecord {
  t :: Float,
  p :: Vec3,
  normal :: Vec3
} deriving (Show, Eq)

class Hitable a where
  hit :: a -> Ray -> Float -> Float -> Maybe HitRecord

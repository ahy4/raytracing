{-# LANGUAGE NoImplicitPrelude #-}

module Hitable where

import Prelude (Float, Bool, ($), Show, Maybe)
import Vec3
import Ray

data HitRecord = HitRecord {
  t :: Float,
  p :: Vec3 Float,
  normal :: Vec3 Float
}

class Hitable a where
  hit :: a -> Ray -> Float -> Float -> Maybe HitRecord

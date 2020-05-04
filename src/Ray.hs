{-# LANGUAGE NoImplicitPrelude #-}

module Ray (Ray(..), pointAtParameter) where

import Prelude (Float, ($), Show)
import Vec3

data Ray = Ray {
  origin :: Vec3 Float,
  direction :: Vec3 Float
} deriving (Show)

pointAtParameter :: Ray -> Float -> Vec3 Float
pointAtParameter ray t = origin ray +: scale t (direction ray)

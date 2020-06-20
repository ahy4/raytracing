module Ray (Ray(..), pointAtParameter) where

import Prelude (Float, ($), Show)
import Vec3

data Ray = Ray {
  origin :: Vec3,
  direction :: Vec3
} deriving (Show)

pointAtParameter :: Ray -> Float -> Vec3
pointAtParameter ray t = origin ray +: scale t (direction ray)

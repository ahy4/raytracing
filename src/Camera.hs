module Camera where

import Vec3
import Ray
import Prelude (Float)

lowerLeftCorner = Vec3 (-2.0) (-1.0) (-1.0)
horizontal = Vec3 4 0 0
vertical = Vec3 0 2 0
originPoint = Vec3 0 0 0

getRay :: Float -> Float -> Ray
getRay u v = Ray
  { origin=originPoint
  , direction=lowerLeftCorner +: scale u horizontal +: scale v vertical -: originPoint }

module Sphere where

import Prelude
import Vec3
import Ray
import Hitable

data Sphere = Sphere {
  center :: Vec3 Float,
  radius :: Float
}

instance Hitable Sphere where
  hit sphere ray tMin tMax
    | discriminant <= 0 = Nothing
    | tMin <= t1 && t1 <= tMax = Just $ hitRecord t1
    | tMin <= t2 && t2 <= tMax = Just $ hitRecord t2
    | otherwise = Nothing
    where
      oc = origin ray -: center sphere
      a = squaredLength $ direction ray
      b = oc .: direction ray
      c = squaredLength oc - radius sphere ** 2
      discriminant = b ** 2 - a * c
      t1 = (-b - sqrt discriminant) / a
      t2 = (-b + sqrt discriminant) / a
      p = pointAtParameter ray
      normal t = scale (1 / radius sphere) $ p t -: center sphere
      hitRecord t = HitRecord t (p t) (normal t)

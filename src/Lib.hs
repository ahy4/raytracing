{-# LANGUAGE NoImplicitPrelude #-}

module Lib ( someFunc ) where

import Data.List
import Data.Maybe
import Vec3
import Ray (Ray(..), pointAtParameter)
import Prelude (IO, Int, String, Float, Bool, writeFile, show, floor, ($), (<$>), fromIntegral, (-), (+), (*), (/), (**), (>=), (&&), otherwise)
import Hitable
import HitableList
import Sphere

someFunc :: IO ()
someFunc = writeFile "./aaa.ppm" ppmText

width :: Int
width = 200

height :: Int
height = 100

lowerLeftCorner = Vec3 (-2.0) (-1.0) (-1.0)
horizontal = Vec3 4 0 0
vertical = Vec3 0 2 0
originPoint = Vec3 0 0 0

spheres =
  [
    Sphere {
      center=Vec3 0 0 (-1),
      radius=0.5
    },
    Sphere {
      center=Vec3 0 (-100.5) (-1),
      radius=100
    }
  ]

world = HitableList spheres

color :: Hitable a => Ray -> a -> Vec3 Float
color ray hitable
  | isHit     = scale 0.5 $ normal (fromJust hitResult) +: Vec3 1 1 1
  | otherwise = scale (1.0 - t) (Vec3 1 1 1) +: scale t (Vec3 0.5 0.7 1)
  where
    unitDirection = unitVector $ direction ray
    t = (*) 0.5 $ y unitDirection + 1.0
    hitResult = hit hitable ray 0 1000000
    isHit = isJust hitResult

toFloat :: Int -> Float
toFloat n = fromIntegral n :: Float

ppmText :: String
ppmText = header ++ body ++ "\n"
  where
    header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body = intercalate "\n" [ generateRgb $ color ray world |
        y <- reverse [0 .. previous height],
        x <- [0 .. previous width],
        let u = x / toFloat width,
        let v = y / toFloat height,
        let ray = Ray {
          origin=originPoint,
          direction=lowerLeftCorner +: scale u horizontal +: scale v vertical }]
    previous = (+) (-1) <$> toFloat

generateRgb :: Vec3 Float -> String
generateRgb (Vec3 r g b) = unwords [scale r, scale g, scale b]
  where
    scale = show <$> floor <$> (*) 255.99

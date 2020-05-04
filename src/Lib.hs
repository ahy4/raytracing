{-# LANGUAGE NoImplicitPrelude #-}

module Lib ( someFunc ) where

import Data.List
import Vec3
import Prelude (IO, Int, String, Float, writeFile, show, floor, ($), (<$>), fromIntegral)

someFunc :: IO ()
someFunc = writeFile "./aaa.ppm" ppmText

width :: Int
width = 200

height :: Int
height = 100

toFloat :: Int -> Float
toFloat n = fromIntegral n :: Float

ppmText :: String
ppmText = header ++ body ++ "\n"
  where
    header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
    body = intercalate "\n" [
      generateRgb $ Vec3 (x `divide` toFloat width) (y `divide` toFloat height) 0.2 |
        y <- reverse [0 .. previous height],
        x <- [0 .. previous width] ]
    previous = plus (-1) <$> toFloat

generateRgb :: Vec3 Float -> String
generateRgb (Vec3 r g b) = unwords [scale r, scale g, scale b]
  where
    scale = show <$> floor <$> multiply 255.99

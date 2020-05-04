module Lib
    ( someFunc
    ) where

import Data.List

someFunc :: IO ()
someFunc = writeFile "./aaa.ppm" ppmText

width :: Int
width = 200

height :: Int
height = 100

ppmText :: String
ppmText = ppmHeader ++ ppmBody ++ "\n"
  where
    ppmBody = intercalate "\n" [
      generateRgb (divide x width) (divide y height) 0.2 |
        y <- reverse [0..height-1],
        x <- [0..width-1] ]
    divide a b = (fromIntegral a::Float) / (fromIntegral b::Float)

ppmHeader :: String
ppmHeader = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"

generateRgb :: Float -> Float -> Float -> String
generateRgb r g b = ir ++ " " ++ ig ++ " " ++ ib
  where
    ir = show $ floor $ 255.99 * r
    ig = show $ floor $ 255.99 * g
    ib = show $ floor $ 255.99 * b

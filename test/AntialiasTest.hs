module AntialiasTest where

import Test.Hspec
import Prelude
import Camera
import Vec3
import Color
import Data.Maybe
import Antialias
import System.Random

test :: IO ()
test = hspec $ do

  let colorFn (x, y) _ = mkColor $ unitVector $ Vec3 x y (x+y/2)
  let gen = mkStdGen 7

  it "antialias should create Just" $ do
    colorFn (1, 2) gen `shouldSatisfy` isJust
    antialias 2 colorFn (1, 2) gen `shouldSatisfy` isJust
    colorFn (1000000, 0) gen `shouldSatisfy` isJust
    antialias 2 colorFn (1000000, 0) gen `shouldSatisfy` isJust

  it "antialias should create Nothing when colorFn create Nothing" $ do
    colorFn (100, -1) gen `shouldSatisfy` isNothing
    antialias 2 colorFn (100, -1) gen `shouldSatisfy` isNothing

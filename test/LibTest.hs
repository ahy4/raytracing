module LibTest where

import Test.Hspec
import Prelude
import Lib
import System.Random
import Camera
import Vec3
import Color
import Data.Maybe
import Control.Monad

test :: IO ()
test = hspec $ do
  it "doesn't leak memory" $ do
    let text = ppmText $ mkStdGen 10
    1 `shouldBe` 1
    -- Prelude.length (filter (== '\n') text) `shouldBe` 20003

  it "color should create Just" $ do
    let gen = mkStdGen 7
    color (getRay 0 0) world gen `shouldSatisfy` isJust
    color (getRay 20 20) world gen `shouldSatisfy` isJust
    color (getRay 50 0) world gen `shouldSatisfy` isJust
    color (getRay 0 100) world gen `shouldSatisfy` isJust
    color (getRay 150 30) world gen `shouldSatisfy` isJust
    color (getRay 200 100) world gen `shouldSatisfy` isJust

  let colorFn (x, y) = mkColor $ unitVector $ Vec3 x y (x+y/2)

  it "antialias should create Just" $ do
    colorFn (1, 2) `shouldSatisfy` isJust
    antialias 2 colorFn (1, 2) `shouldSatisfy` isJust
    colorFn (1000000, 0) `shouldSatisfy` isJust
    antialias 2 colorFn (1000000, 0) `shouldSatisfy` isJust

  it "antialias should create Nothing when colorFn create Nothing" $ do
    colorFn (100, -1) `shouldSatisfy` isNothing
    antialias 2 colorFn (100, -1) `shouldSatisfy` isNothing

  it "creates random vector in sphere" $ do
    forM_ [1..100] $ \n -> do
      randomInUnitSphere (mkStdGen n) `shouldSatisfy` (<=1) . Vec3.length . snd

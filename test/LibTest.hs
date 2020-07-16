module LibTest where

import Test.Hspec
import Prelude
import Lib
import System.Random
import Camera
import Vec3
import Data.Maybe

test :: IO ()
test = hspec $ do
  it "doesn't leak memory" $ do
    let text = ppmText $ mkStdGen 10
    Prelude.length (filter (== '\n') (fromJust text)) `shouldBe` 20003

  it "color should create Just" $ do
    let gen = mkStdGen 7
    color (getRay 0 0) world gen `shouldSatisfy` isJust
    color (getRay 20 20) world gen `shouldSatisfy` isJust
    color (getRay 50 0) world gen `shouldSatisfy` isJust
    color (getRay 0 100) world gen `shouldSatisfy` isJust
    color (getRay 150 30) world gen `shouldSatisfy` isJust
    color (getRay 200 100) world gen `shouldSatisfy` isJust

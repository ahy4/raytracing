module LibTest where

import Test.Hspec
import Prelude
import Lib
import System.Random
import Camera
import Vec3

test :: IO ()
test = hspec $ do
  it "doesn't leak memory" $ do
    let text = ppmText $ mkStdGen 10
    Prelude.length (filter (== '\n') text) `shouldBe` 20003

  it "should be proper color" $ do
    color (getRay 0 0) world `shouldBe` Vec3 127 179 255

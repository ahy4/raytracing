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
    toRgbText (color (getRay 0 0) world) `shouldBe` "165 201 255"
    toRgbText (color (getRay 50 30) world) `shouldBe` "175 207 255"

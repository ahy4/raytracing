module ColorTest where

import Test.Hspec
import Prelude
import Color
import Data.Maybe
import Vec3

test :: IO ()
test = hspec $ do
  it "creates Just color" $ do
    mkColor (Vec3 0.5 0.5 0.5) `shouldSatisfy` isJust
    mkColor (Vec3 0 0 0) `shouldSatisfy` isJust
    mkColor (Vec3 0.99 0.99 0.99) `shouldSatisfy` isJust
    mkColor (Vec3 1 0 0) `shouldSatisfy` isJust

  it "creates Nothing" $ do
    mkColor (Vec3 0 (-0.1) 0) `shouldSatisfy` isNothing
    mkColor (Vec3 1.0000001 0 0) `shouldSatisfy` isNothing
    mkColor (Vec3 0.8 0.2 100) `shouldSatisfy` isNothing

  it "generates RGB string" $ do
    let col = fromJust $ mkColor $ Vec3 0.5 0.5 0.5
    toRgbText col `shouldBe` "127 127 127"

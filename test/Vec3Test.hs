module Vec3Test where

import Test.Hspec
import Prelude
import Vec3

test :: IO ()
test = hspec $ do
  it "add vectors" $ do
    Vec3 1 2 3 +: Vec3 4 5 6 `shouldBe` Vec3 5 7 9
    Vec3 (-1) (-2) (-3) +: Vec3 4 5 6 `shouldBe` Vec3 3 3 3
    Vec3 1.2 3.4 5.6 +: Vec3 7.8 9.1 2.3 `shouldBe` Vec3 9 12.5 7.8999996

  it "sub vectors" $ do
    Vec3 1 2 3 -: Vec3 4 5 6 `shouldBe` Vec3 (-3) (-3) (-3)
    Vec3 1 2 3 +: Vec3 (-4) 5 6 `shouldBe` Vec3 (-3) 7 9

  it "multiply vectors" $ do
    Vec3 1 2 3.2 *: Vec3 4 5 6 `shouldBe` Vec3 4 10 19.2

  it "divide vectors" $ do
    Vec3 4 5 6 /: Vec3 1 2 3 `shouldBe` Vec3 4 2.5 2
    let divByZero = Vec3 1 0 1 /: Vec3 0 0 (-0)
    x divByZero `shouldSatisfy` isInfinite
    x divByZero `shouldSatisfy` (>0)
    y divByZero `shouldSatisfy` isNaN
    z divByZero `shouldSatisfy` isInfinite
    z divByZero `shouldSatisfy` (<0)

  it "scale vectors" $ do
    scale 3 (Vec3 1 2 3) `shouldBe` Vec3 3 6 9

  it "cross vectors" $ do
    Vec3 1 2 3 `cross` Vec3 4 5 6 `shouldBe` Vec3 (-3) 6 (-3)

  it "vector distance" $ do
    squaredLength (Vec3 1 2 3) `shouldBe` 14
    Vec3.length (Vec3 0 3 (-4)) `shouldBe` 5

  it "gets unit vector" $ do
    unitVector (Vec3 0 3 4) `shouldBe` Vec3 0 (3/5) (4/5)

  it "gets vector average" $ do
    average [Vec3 1 2 3, Vec3 4 5 6, Vec3 7 8 9] `shouldBe` Vec3 4 5 6

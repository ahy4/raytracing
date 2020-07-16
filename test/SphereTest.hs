module SphereTest where

import Test.Hspec
import Prelude
import Sphere
import Vec3
import Ray
import Hitable
import Data.Maybe

test :: IO ()
test = hspec $ do
  it "hit the ray to sphere" $ do
    let sphere = Sphere (Vec3 2 2 0) 1
    let ray = Ray (Vec3 0 0 0) (Vec3 1 1 0)
    let hitResult = hit sphere ray 0 10000000
    hitResult `shouldSatisfy` isJust
    let hitRecord = fromJust hitResult
    p hitRecord `shouldBe` Vec3 1.2928932 1.2928932 0.0
    normal hitRecord `shouldBe` Vec3 (-0.7071068) (-0.7071068) 0.0

  it "doesn't hit the ray to sphere" $ do
    let sphere = Sphere (Vec3 2 2 0) 1
    let ray = Ray (Vec3 0 4 0) (Vec3 5 0 0)
    hit sphere ray 0 10000000 `shouldSatisfy` isNothing

  it "when touch the ray to sphere, mark as unhit" $ do
    let sphere = Sphere (Vec3 2 2 0) 1
    let ray = Ray (Vec3 0 3 0) (Vec3 5 0 0)
    let hitResult = hit sphere ray 0 10000000
    hitResult `shouldSatisfy` isNothing

  it "doesn't hit because ray is too short" $ do
    let sphere = Sphere (Vec3 2 2 0) 1
    let ray = Ray (Vec3 0 3 0) (Vec3 5 0 0)
    let hitResult = hit sphere ray 0 0.3
    hitResult `shouldSatisfy` isNothing

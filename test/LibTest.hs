module LibTest where

import Test.Hspec
import Prelude
import Lib
import System.Random
import Camera
import Vec3

test :: IO ()
test = hspec $
  it "doesn't leak memory" $ do
    -- let gen = mkStdGen 10
    -- ppmText gen `shouldSatisfy` (>0) . length
    color (getRay 0 0) world `shouldSatisfy` (>0) . Vec3.length
    tmp `shouldSatisfy` not . null
    Prelude.length tmp `shouldSatisfy` (==20000)
    -- color (getRay 50 0) world `shouldSatisfy` (>0) . Vec3.length
    -- color (getRay 0 50) world `shouldSatisfy` (>0) . Vec3.length
    -- color (getRay 50 50) world `shouldSatisfy` (>0) . Vec3.length
    -- color (getRay 20 20) world `shouldSatisfy` (>0) . Vec3.length
    -- color (getRay 80 80) world `shouldSatisfy` (>0) . Vec3.length

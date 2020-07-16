module RandomUtilTest where

import Test.Hspec
import Prelude
import System.Random
import RandomUtil
import Vec3
import Control.Monad

test :: IO ()
test = hspec $ do

  it "creates random vector in sphere" $ do
    forM_ [1..100] $ \n -> do
      randomInUnitSphere (mkStdGen n) `shouldSatisfy` (<=1) . Vec3.length . snd

  it "creates random generators" $ do
    let g1 = mkStdGen 7
    let result = take 10 $ createRandomGeneratorsLazy g1
    show result `shouldBe` "[9 40692,320113 2147442707,2071543754 2147402015,33684306 2147361323,1371586270 2147320631,1563031739 2147279939,1988159084 2147239247,668955828 2147198555,1363332347 2147157863,2003025519 2147117171]"

import Prelude
import Test.Hspec
import SphereTest
import Vec3Test
import LibTest

main :: IO ()
main = do
  SphereTest.test
  Vec3Test.test
  LibTest.test

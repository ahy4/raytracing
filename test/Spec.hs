import Prelude
import Test.Hspec
import SphereTest
import Vec3Test

main :: IO ()
main = do
  SphereTest.test
  Vec3Test.test

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified TestPlanner
import qualified TestStore
import qualified TestTypes
import qualified TestVerify

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Techtonica"
  [ TestPlanner.tests
  , TestStore.tests
  , TestTypes.tests
  , TestVerify.tests
  ]


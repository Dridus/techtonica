module TestPlanner where

import ArbitraryTypes ()
import Data.Graph.Inductive (LEdge, LNode, mkGraph)
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import Prettyprinter (pretty)
import Tech.Planner
import Tech.Pretty (ppAnonymousClusterDy)
import Tech.TestFixtures (expectedLinearDy, linearSt, testItemA, testItemB, testMachine)
import Tech.Types
import Test.QuickCheck (Positive (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)
import TestUtils

tests :: TestTree
tests =
  testGroup
    "Planner"
    [ testAssignClusterRates
    , testEstimate
    ]

testAssignClusterRates :: TestTree
testAssignClusterRates =
  testGroup
    "assignClusterRates"
    [ quantityProportional
    , cycleTimeInverseProportional
    ]
 where
  quantityProportional = testProperty "rates proportional to quantity" $
    \(Positive qty) ->
      peqGraph
        feqClusterDy
        ppAnonymousClusterDy
        id
        (==)
        pretty
        (assignClusterRates (mkGraph [trivialClSt 1.0 qty] []))
        (mkGraph [trivialClDy 1.0 qty (Rate $ 10.0 * unQuantity qty) (Rate $ unQuantity qty)] edges)
  cycleTimeInverseProportional = testProperty "rates inversely proportional to cycle time" $
    \(Positive (secondsToNominalDiffTime -> time)) ->
      peqGraph
        feqClusterDy
        ppAnonymousClusterDy
        id
        (==)
        pretty
        (assignClusterRates (mkGraph [trivialClSt time 1.0] []))
        (mkGraph [trivialClDy time 1.0 (Rate $ 10.0 / realToFrac time) (Rate $ 1.0 / realToFrac time)] edges)
  trivialRecipe time =
    Recipe
      (RecipeKey testMachine "trivial")
      time
      ((testItemA, Quantity 10.0) :->-: (testItemB, Quantity 1.0))
  trivialClDy :: NominalDiffTime -> Quantity -> Rate -> Rate -> LNode ClusterDy
  trivialClDy time qty ein eout =
    (1,) $
      ClusterDy
        (trivialRecipe time)
        qty
        ((testItemA, ein) :->-: (testItemB, eout))
  trivialClSt :: NominalDiffTime -> Quantity -> LNode ClusterSt
  trivialClSt time qty = (1,) $ ClusterSt (trivialRecipe time) qty
  edges :: [LEdge ()]
  edges = []

testEstimate :: TestTree
testEstimate =
  testGroup "estimate" [linearNetwork]
 where
  linearNetwork =
    testCase "linear network" $
      factoryDyShouldBe (estimate linearSt) expectedLinearDy

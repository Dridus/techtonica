module TestPlanner where

import ArbitraryTypes ()
import Data.Graph.Inductive (LEdge, LNode, mkGraph)
import Data.Time.Clock (NominalDiffTime, secondsToNominalDiffTime)
import Prettyprinter (pretty)
import Tech.Planner.Estimate
import Tech.Pretty (ppAnonymousClusterDy)
import Tech.TestFixtures (expectedLinearDy, linearSt, testItemA, testItemB, testMachineId)
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
    [ testEstimatePlanner
    , testProposePlanner
    ]

testEstimatePlanner :: TestTree
testEstimatePlanner =
  testGroup
    "Estimate"
    [ testAssignClusterRates
    , testEstimate
    ]

testAssignClusterRates :: TestTree
testAssignClusterRates =
  testGroup
    "assignClusterRates"
    [ quantityProportional
    , cycleTimeInverseProportional
    , parallelismProportional
    ]
 where
  peqAssigned = peqGraph feqClusterDy ppAnonymousClusterDy id (==) pretty
  quantityProportional = testProperty "rates proportional to quantity" $
    \(Positive qty) ->
      assignClusterRates (mkGraph [trivialClSt 60.0 1.0 qty] [])
        `peqAssigned` mkGraph
          [ trivialClDy
              60.0
              1.0
              qty
              (PerMinute $ 10.0 * unQuantity qty)
              (PerMinute $ unQuantity qty)
          ]
          edges
  cycleTimeInverseProportional = testProperty "rates inversely proportional to cycle time" $
    \(Positive (secondsToNominalDiffTime -> time)) ->
      assignClusterRates (mkGraph [trivialClSt time 1.0 1.0] [])
        `peqAssigned` mkGraph
          [ trivialClDy
              time
              1.0
              1.0
              (PerMinute $ 10.0 / realToFrac (time / 60))
              (PerMinute $ 1.0 / realToFrac (time / 60))
          ]
          edges

  parallelismProportional = testProperty "rates proportional to parallelism" $
    \(Positive par) ->
      assignClusterRates (mkGraph [trivialClSt 60.0 par 1.0] [])
        `peqAssigned` mkGraph [trivialClDy 60.0 par 1.0 (PerMinute $ 10.0 * par) (PerMinute par)] edges
  trivialRecipe time =
    Recipe
      (RecipeKey testMachineId "trivial")
      time
      ([(testItemA, Quantity 10.0)] :>>: [(testItemB, Quantity 1.0)])
  trivialMachine :: Rational -> Machine
  trivialMachine = Machine testMachineId
  trivialClDy :: NominalDiffTime -> Rational -> Quantity -> PerMinute -> PerMinute -> LNode ClusterDy
  trivialClDy time par qty ein eout =
    (1,) $
      ClusterDy
        (snd $ trivialClSt time par qty)
        ([(testItemA, ein)] :>>: [(testItemB, eout)])
  trivialClSt :: NominalDiffTime -> Rational -> Quantity -> LNode ClusterSt
  trivialClSt time par qty = (1,) $ ClusterSt (trivialRecipe time) (trivialMachine par) qty
  edges :: [LEdge ()]
  edges = []

testEstimate :: TestTree
testEstimate =
  testGroup "estimate" [linearNetwork]
 where
  linearNetwork =
    testCase "linear network" $
      factoryDyShouldBe (estimate linearSt) expectedLinearDy

testProposePlanner :: TestTree
testProposePlanner =
  testGroup
    "Propose"
    []

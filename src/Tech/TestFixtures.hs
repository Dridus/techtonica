module Tech.TestFixtures where

import Data.Graph.Inductive (Gr, mkGraph)
import Data.Set qualified as Set
import Tech.Machines (externalSink, externalSource)
import Tech.Planner.Estimate (
  assignClusterRates,
  estimate,
  externalClusterDy,
 )
import Tech.Recipes (Recipes, indexRecipes)
import Tech.Types

testItemA, testItemB, testItemC :: Item
testItemA = Item "testA"
testItemB = Item "testB"
testItemC = Item "testC"

testItems :: Set Item
testItems = Set.fromList [testItemA, testItemB, testItemC]

testMachine :: Machine
testMachine = Machine "test"

testTransferA1B1, testTransferB1C1 :: Num q => Transfer q
testTransferA1B1 = [(testItemA, 1)] :>>: [(testItemB, 1)]
testTransferB1C1 = [(testItemB, 1)] :>>: [(testItemC, 1)]

testRecipeA1B1, testRecipeB1C1 :: Recipe
testRecipeA1B1 = Recipe (RecipeKey testMachine "a1b1") 1 testTransferA1B1
testRecipeB1C1 = Recipe (RecipeKey testMachine "b1c1") 1 testTransferB1C1

testRecipes :: Recipes
testRecipes = indexRecipes [testRecipeA1B1, testRecipeB1C1]

testClusterA1B1, testClusterB1C1 :: ClusterSt
testClusterA1B1 = ClusterSt testRecipeA1B1 1
testClusterB1C1 = ClusterSt testRecipeB1C1 1

externalSourceDy :: Image Rate -> ClusterDy
externalSourceDy outs = externalClusterDy externalSource (Transfer mempty outs)

externalSinkDy :: Image Rate -> ClusterDy
externalSinkDy ins = externalClusterDy externalSink (Transfer ins mempty)

linearSt :: FactorySt
linearSt =
  mkGraph
    [ (1, testClusterA1B1)
    , (2, testClusterB1C1)
    ]
    [ (1, 2, BeltSt testItemB)
    ]

linearAr :: Gr ClusterDy BeltSt
linearAr = assignClusterRates linearSt

linearDy :: FactoryDy
linearDy = estimate linearSt

expectedLinearDy :: FactoryDy
expectedLinearDy =
  mkGraph
    [ (1, ClusterDy testClusterA1B1 testTransferA1B1)
    , (2, ClusterDy testClusterB1C1 testTransferB1C1)
    , (-1, externalSourceDy (One (testItemA, 1)))
    , (-2, externalSinkDy (One (testItemC, 1)))
    ]
    [ (1, 2, BeltDy (BeltSt testItemB) 1 1)
    , (-1, 1, BeltDy (BeltSt testItemA) 1 1)
    , (2, -2, BeltDy (BeltSt testItemC) 1 1)
    ]

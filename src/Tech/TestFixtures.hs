module Tech.TestFixtures where

import Data.Graph.Inductive (Gr, LNode, mkGraph)
import Tech.Planner (assignClusterRates, estimate, externalRecipe, nExternalSink, nExternalSource)
import Tech.Types

testItemA, testItemB, testItemC :: Item
testItemA = Item "testA"
testItemB = Item "testB"
testItemC = Item "testC"

testMachine :: Machine
testMachine = Machine "test"

testTransferA1B1, testTransferB1C1 :: Num q => Transfer q
testTransferA1B1 = (testItemA, 1) :->-: (testItemB, 1)
testTransferB1C1 = (testItemB, 1) :->-: (testItemC, 1)

testRecipeA1B1, testRecipeB1C1 :: Recipe
testRecipeA1B1 = Recipe (RecipeKey testMachine "a1b1") 1 testTransferA1B1
testRecipeB1C1 = Recipe (RecipeKey testMachine "b1c1") 1 testTransferB1C1

testClusterA1B1, testClusterB1C1 :: ClusterSt
testClusterA1B1 = ClusterSt testRecipeA1B1 1
testClusterB1C1 = ClusterSt testRecipeB1C1 1

externalSource :: Image Rate -> LNode ClusterDy
externalSource outs = (nExternalSource, ClusterDy externalRecipe 0.0 (Transfer mempty outs))

externalSink :: Image Rate -> LNode ClusterDy
externalSink ins = (nExternalSink, ClusterDy externalRecipe 0.0 (Transfer ins mempty))

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
    [ (1, ClusterDy testRecipeA1B1 1 testTransferA1B1)
    , (2, ClusterDy testRecipeB1C1 1 testTransferB1C1)
    , externalSource (One (testItemA, 1))
    , externalSink (One (testItemC, 1))
    ]
    [ (1, 2, BeltDy testItemB 1 1)
    , (nExternalSource, 1, BeltDy testItemA 1 1)
    , (2, nExternalSink, BeltDy testItemC 1 1)
    ]
